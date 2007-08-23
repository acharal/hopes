module Tc (
        module Tc,
        module TcMonad
    ) where

import TcMonad
import Types
import HpSyn
import Err
import Loc
import Pretty
import Monad    (zipWithM, zipWithM_, when)
import List     (nub)
import Maybe    (catMaybes)

import Control.Monad.Reader (asks)
import Control.Monad.State  (gets)


tcSource :: HpSource -> Tc (HpSource, TypeEnv)
tcSource src = do
    let tysign = map (\(L _ (HpTySig v t)) -> (v, t)) (tysigs src)
    let cls    = clauses src
    let maybepred (L _ (HpPred p)) = Just p
        maybepred _ = Nothing
    let preds  = nub $ catMaybes $ map (maybepred.headA.hLit) cls
    tv <- mapM initNewTy preds
    extendEnv tv $ do
    extendEnv tysign $ do
        cls'    <- mapM (\c -> tcWithLoc c (tcClause c)) cls
        ty_env  <- asks tyenv >>= normEnv
        return (src { clauses = cls' }, ty_env)



-- type checking and inference

tcClause, tcClause' :: LHpClause -> Tc LHpClause

tcClause clause =
    tcWithCtxt (clauseCtxt clause) $ tcClause' clause

tcClause' c@(L loc (HpClaus h b)) = do
    tvs <- mapM initNewTy (bvarS c)
    extendEnv tvs $ do
        h' <- tcAtom h
        b' <- mapM tcAtom b
        return (L loc (HpClaus h' b'))

tcGoal :: LHpGoal -> Tc LHpGoal
tcGoal (L loc g) = do
    tvs <- mapM initNewTy (bvarSL g)
    extendEnv tvs $ do
        g' <- mapM tcAtom g
        return (L loc g')

tcAtom :: LHpAtom -> Tc LHpAtom
tcAtom atom = 
    tcWithCtxt (atomCtxt atom) $ tcAtom' atom

tcAtom' :: LHpAtom -> Tc LHpAtom
tcAtom' (L loc (HpAtom e)) = do
    e' <- tcExpr e tyBool   -- force atom to be of type Bool
    return (L loc (HpAtom e'))

tcAtom' e = return e

tiExpr :: LHpExpr -> Tc MonoType
tiExpr e = do
    var_ty <- newTyVar
    tcExpr e var_ty
    return var_ty

tcExpr :: LHpExpr -> MonoType -> Tc LHpExpr

tcExpr (L loc (HpTerm t)) exp_ty = do
    t' <- tcTerm t exp_ty
    return (L loc (HpTerm t'))

tcExpr (L loc (HpPar  e)) exp_ty = do
    e' <- tcExpr e exp_ty
    return (L loc (HpPar e'))

tcExpr (L loc (HpAnno e ty)) exp_ty = do
    ann_ty <- instantiate ty
    e' <- tcExpr e ann_ty
    unify ann_ty exp_ty
    return (L loc (HpAnno e' ty))

tcExpr (L loc (HpApp e args)) exp_ty = do
    fun_ty <- tiExpr e
    (args_ty, res_ty) <- unifyFun (length args) fun_ty
    args' <- zipWithM tcExpr args args_ty
    unify res_ty exp_ty
    return (L loc (HpApp e args'))

tcExpr (L loc (HpPred p)) exp_ty = do
    pred_ty <- instantiate =<< lookupVar p
    unify pred_ty exp_ty
    return (L loc (HpPred p))

tcTerm term exp_ty = do
    tcWithCtxt (termCtxt term) $ tcTerm' term exp_ty

tcTerm' (L loc (HpCon c)) exp_ty = do
    unify tyAll exp_ty
    return (L loc (HpCon c))

tcTerm' (L loc (HpVar v)) exp_ty = do
    var_ty <- lookupVar v
    unify var_ty exp_ty
    return (L loc (HpVar v))

tcTerm' (L loc (HpFun f tl)) exp_ty = do
    tl' <- mapM (\t -> tcTerm t tyAll) tl 
    unify tyAll exp_ty
    return (L loc (HpFun f tl'))

tcTerm' (L loc (HpList tl maybe_t)) exp_ty = do
    tl' <- mapM (\t -> tcTerm t tyAll) tl
    maybe_t' <- case maybe_t of
                    Nothing -> return Nothing
                    Just t ->  tcTerm t tyAll >>= return.Just
    unify tyAll exp_ty
    return (L loc (HpList tl' maybe_t'))

tcTerm' (L loc (HpTup tl)) exp_ty = do
    tl' <- mapM (\t -> tcTerm t tyAll) tl
    unify tyAll exp_ty
    return (L loc (HpTup tl'))

tcTerm' t@(L loc HpWild) exp_ty = return t

-- unification

unify :: MonoType -> MonoType -> Tc ()
unify (TyVar v1) t@(TyVar v2)
    | v1 == v2    = return ()
    | otherwise   = unifyVar v1 t

unify (TyVar v) t = unifyVar v t
unify t (TyVar v) = unifyVar v t

unify (TyFun fun1 arg1) (TyFun fun2 arg2) =
    unify fun1 fun2 >> unify arg1 arg2

unify t@(TyTup tys) t'@(TyTup tys')
    | length tys == length tys' = zipWithM_ unify tys tys'
    | otherwise                 = unificationErr t t'
unify t t'
    | t == t'     = return ()
    | otherwise   = unificationErr t t'


unifyFun n t = do
    arg_tys <- sequence $ replicate n newTyVar
    res_ty <- newTyVar
    let args_ty = if n > 1 then TyTup arg_tys else (head arg_tys)
    unify (TyFun args_ty res_ty) t
    return (arg_tys, res_ty)

unifyVar v t = do
    maybe_ty <- lookupTyVar v
    case maybe_ty of
        Nothing -> varBind v t
        Just t' -> unify t' t

varBind  v1 t@(TyVar v2) = do
    maybe_ty <- lookupTyVar v2
    case maybe_ty of
        Nothing -> addConstraint v1 t
        Just ty -> unify (TyVar v1) ty

varBind v ty = do
    tvs <- getTyVars ty
    if (v `elem` tvs) then occurCheckErr v ty else addConstraint v ty


-- utilities

instantiate :: Type -> Tc MonoType
instantiate t = return t

generalize :: MonoType -> Tc Type
generalize t = return t

getTyVars :: MonoType -> Tc [TyVar]
getTyVars (TyVar v) = do
    maybe_ty <- lookupTyVar v
    case maybe_ty of
        Nothing -> return [v]
        Just ty' -> getTyVars ty' >>= \tvs' -> return (v:tvs')

getTyVars (TyFun ty1 ty2) = do
    tvs1 <- getTyVars ty1
    tvs2 <- getTyVars ty2
    return (tvs1 ++ tvs2)

getTyVars (TyTup tl) = do
    l <- mapM getTyVars tl
    return (concat l)

getTyVars (TyCon _) = return []


normEnv :: TypeEnv -> Tc TypeEnv
normEnv env = 
    let aux (v,t) = do
            t' <- normTy t
            return (v, t')
    in  mapM aux env

normTy (TyFun t1 t2) = do
    t1' <- normTy t1
    t2' <- normTy t2
    return $ TyFun t1' t2'

normTy (TyVar tv) = do
    ty <- lookupTyVar tv
    case ty of
        Just t -> normTy t
        Nothing -> return $ TyVar tv

normTy (TyTup tl) = do
    tl' <- mapM (\t -> normTy t) tl
    return $ TyTup tl'

normTy t = return t

initNewTy v = do
    ty <- newTyVar >>= generalize
    return (v, ty)

-- error reporting

unificationErr inf_ty exp_ty = do
        inf_ty' <- normTy inf_ty
        exp_ty' <- normTy exp_ty
        let desc = hang (text "Could not match types:") 4 
                        (vcat [ text "Inferred:" <+> ppr inf_ty', 
                                text "Expected:" <+> ppr exp_ty' ])
        typeError desc


occurCheckErr tv ty = 
    typeError (text "Could not construct an infinite type")


-- error contexts
clauseCtxt lcl@(L loc cl) = 
    hang (if fact lcl then text "In fact:" else text "In rule:") 4 (ppr cl)
atomCtxt (L loc atom) = hang (text "In atom:") 4 (ppr atom)
termCtxt (L loc term) = hang (text "In term:") 4 (ppr term)
