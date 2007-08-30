module Wffc where

import HpSyn
import TcMonad
import Tc
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
        4.1 e must not be bounded variable.

    II. Restrictions on the body {L_i}

    Let L_i an atom in B of form L(t_1, ..., t_n)

    5. if t_i is an application f(t'_1, ..., t'_k) must be of type i. Moreover,
       t'_i must be of type i. No (partial) application of relations allowed.
       a function symbol must be of type (i, ..., i) -> i. No ho expressions allowed
       as arguments of function symbols.

    III. Zonking Environment

    1. No polymorfism allowed. If any type variable exist must be set to type i.

    IV. Higher Order Expressions

    1. After the type checking, some expressions are polymorphic, meaning that these
       expression can be either higher order or zero order.

    V. Strategy

    1. Restrict as much as you can the types. Apply first checks that function
       symbols must be of type i.
    2. Do the 3 check. Find all variables that occur more than once in the head and
       *restrict* them to be of type i (because they can't be higher order)
    4. Check any higher order expressions occurs other than bounded variables.
    5. Check no1 can be done whenever.

    VI. Utilities

    1. arguments of an application  - done
    2. check if an expression is an application - easy
    3. match expressions with types
    4. 
-}

-- wffcSource :: (HpSource a, TypeEnv) -> Tc (HpSource a, TypeEnv)
wffcSource (src, tyenv) = do
    extendEnv tyenv $ do
        mapM_ (\c -> withLocation c (wffcClause c)) (clauses src)
        --mapM_ wffcClause (clauses src)
        ty_env' <- zonkEnv tyenv
        return (src, ty_env')


-- wffcClause :: LHpClause a -> Tc ()
wffcClause cl =
    tcWithCtxt (clauseCtxt cl) $ do
        let b = map binds $ bindings (unLoc cl)
        tvs <- mapM initNewTy b
        extendEnv tvs $ do
            mapM_ checkApps (atomsOf cl)
            checkHead b (hAtom cl)

-- checkApps :: LHpAtom a -> Tc ()
checkApps e = 
    let as = argsOf e
        isapp e = case unLoc e of
                     HpApp _ _ -> True
                     _ -> False
        apps = filter isapp as
    in  do
        mapM_ (\e -> tcExpr e tyAll) apps   -- restrict the result type to be i
        let app_args = concatMap argsOf apps
        mapM_ (\e -> tcExpr e tyAll) $ app_args -- restrict the arguments of the application to be of type i
        mapM_ checkApps app_args -- check recursively the arguments for nested applications


-- checkHead :: [a] -> LHpAtom a -> Tc ()
checkHead bv hd =
    let [hs] = symbolsE $ headOf hd
        eqsym (HpSym s) (HpSym s') = s == s'
        eqsym _ _ = False
    in do
        when (hs `elem` bv) $ varInHead hs      --head is bounded. unexcepted
        -- ty <- lookupVar hs
        -- let etys  = zip (argsOf hd) (tyargs ty)
        -- let bvtys = filter ((\((HpSym x), _) -> x `elem` bv).isSymbol.fst) etys -- bounded variables 
            --bvo = occurences (\x -> \y -> (unLoc x) `eqsym` (unLoc y)) bv'
            -- morethanonce = filter ((>1).snd) bvo
            -- mapM_ (\e -> tcExpr e tyAll) $ map fst $ morethanonce

occurences eq [] = []
occurences eq (x:xs) =
    let (s, r) = partition (eq x) (x:xs)
    in  (x, (length s)):(occurences eq r)


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

multiHoOccurErr occlist =
    typeError (sep ([text "Higher order bound variables",
                     nest 4 (sep (punctuate comma (map (quotes.text.fst) occlist))),
                     text "must occur only once as arguments in the head of a rule"]))

predInHeadErr preds = 
    typeError (sep ([text "Predicate variables",
                     nest 4 (sep (punctuate comma (map (quotes.text) preds))),
                     text "must not occur as arguments in atom in the lhs of a rule"]))


varInHead var = 
    typeError (sep ([text "Bounded variable", 
                     nest 4 (quotes (ppr var)),
                     text "must not occur at the head of atom when in the lhs of rule"]))

