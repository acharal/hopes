module Wffc where

import HpSyn
import TcMonad
import Types
import Err
import Loc
import Pretty
import Monad (when)
import List (partition)


{-
    Well-formated formulas

    1. Well - typed
    2. Definitional

    I. Restrictions on the head atom A

    Let head A of form A = R( t_1, ..., t_n )

    1. R not a bounded variable
    2. t_1 | ... | t_n does not contain any ho expression that is not a bounded variable.
    3. for every t_i and t_j, j != i
       if t_i and t_j are ho bounded variables then t_i != t_j.
       In other words bounded variables that occur in the head *must* be distinct.
    4. if t_i is an application e(t'_1, ... t'_k) then must be of type i and every
       t'_i must be of type i. Applications (or partial applications) of relations e
       must not occur in head.

    II. Restrictions on the body {L_i}

    Let L_i an atom in B of form L(t_1, ..., t_n)

    5. if t_i is an application f(t'_1, ..., t'_k) must be of type i. Moreover,
       t'_i must be of type i. No (partial) application of relations allowed.
       a function symbol must be of type (i, ..., i) -> i. No ho expressions allowed
       as arguments of function symbols.

    III. Zonking Environment

    1. No polymorfism allowed. If any type variable exist must be set to type i.

-}


-- check if Source follow the definitional rules of a wfp
-- and zonk the type Environment (namely no tyvar remain)
dfcSource :: (HpSource, TypeEnv) -> Tc (HpSource, TypeEnv)
dfcSource (src,tyenv) = do
    tyenv' <- zonkEnv tyenv
    extendEnv tyenv' $ do
        let doCheck c = recoverTc (dfcClause c) (return ())
        mapM_ doCheck (clauses src)
        failIfErr
        return (src, tyenv')


-- check if HpClause is definitional
dfcClause :: LHpClause -> Tc ()
dfcClause cl@(L loc (HpClaus h b)) = 
    let takeVars ((L _ (HpTerm (L _ (HpVar v)))):rest) = 
            if bound v then v:(takeVars rest) else (takeVars rest)
        takeVars ((L _ _):rest) = takeVars rest
        takeVars [] = []
    in
        tcWithCtxt (clauseCtxt cl) $ dfcLhs cl

-- head must contain distinct higher order variables.
dfcLhs c =
    let h  = hLit c
        ts = argsA h
        takeVars ((L _ (HpTerm (L _ (HpVar v)))):rest) = 
            if bound v then v:(takeVars rest) else (takeVars rest)
        takeVars ((L _ _):rest) = takeVars rest
        takeVars [] = []
    in tcWithCtxt ( atomCtxt (hLit c) ) $ do
       case unLoc (headA h) of 
            (HpPred p) -> do
                ty <- lookupVar p
                let ttys   = tyargs ty
                let hoargs = map fst $ filter (\(v, t) -> order t > 0) $ zip ts ttys
                let occlst = filter ((>1).snd) $ occurences $ takeVars hoargs
                when (not (null occlst)) (multiHoOccurErr occlst)
            (HpTerm (L _ (HpVar v))) -> do
                varInHead v
            _ -> do 
                fail "unexpected error"


occurences [] = []
occurences (x:xs) =
    let (s, r) = partition (==x) (x:xs)
    in  (x, (length s)):(occurences r)


-- head must not contain "other than" higher order variables in higher-order arg positions
zonkEnv :: TypeEnv -> Tc TypeEnv
zonkEnv env = 
    let aux (v,t) = do
            t' <- zonkTy t
            return (v, t')
    in  mapM aux env

zonkTy = zonkTy'

zonkTy' (TyVar _) = return tyAll
zonkTy' (TyFun t1 t2) = do
    t1' <- zonkTy' t1
    t2' <- zonkTy' t2
    return (TyFun t1' t2')
zonkTy' (TyTup tl) = do
    tl' <- mapM zonkTy' tl
    return (TyTup tl')
zonkTy' t = return t

atomCtxt (L loc atom) = hang (text "In atom:") 4 (ppr atom)

clauseCtxt lcl@(L loc cl) = 
    hang (if fact lcl then text "In fact:" else text "In rule:") 4 (ppr cl)

multiHoOccurErr occlist =
    typeError (sep ([text "Higher order bound variables",
                     nest 4 (sep (punctuate comma (map (quotes.text.fst) occlist))),
                     text "must occur only once as arguments in the head of a rule"]))

predInHeadErr preds = 
    typeError (sep ([text "Predicate variables",
                     nest 4 (sep (punctuate comma (map (quotes.text) preds))),
                     text "must not occur as arguments in atom in the lhs of a rule"]))


varInHead var = 
    typeError (sep ([text "Bounded variable", quotes (text var), 
                     text "must not occur at the head of atom when in the lhs of rule"]))

