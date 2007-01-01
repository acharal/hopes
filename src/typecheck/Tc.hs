module Tc where

import HpSyn
import Utils
import Check
import Types
import Maybe
import List 

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Debug.Trace

type Constraint = (TyVar, MonoType)

type TcCS = [Constraint]            -- constraint set

type TypeEnv = [(HpName, Type)]        -- assumption set

type Tc = ErrorT HpError (ReaderT TypeEnv (StateT TcState Identity))

data TcState = TcState { uniq::Int, cs::TcCS }


newTyVar :: Tc MonoType
newTyVar = do
    st <- get
    let n = uniq st
    put st{uniq=n+1}
    return (TyVar n)

extendEnv l m = local (\r -> l++r) m

lookupVar v = do
    env <- ask
    case lookup v env of
        Just a  -> return a
        Nothing -> fail ("Variable "++v++" not in scope")

lookupTyVar tv = do
    st <- get
    return $ lookup tv (cs st)

addConstraint tv ty = modify (\st -> st{cs=(tv,ty):(cs st)})

instantiate :: Type -> Tc MonoType
instantiate t = return t

generalize :: MonoType -> Tc Type
generalize t = return t

runTc m = runIdentity $ runStateT (runReaderT (runErrorT m) []) initState
    where initState = TcState 0 []

tycheckP src =
    let 
        cl = map unLoc (clauses src)
        preds = nub $ catMaybes $ map (getPred.getHead) cl
    in do
        tv <- mapM (\p-> newTyVar >>= \t -> return (p, t)) preds
        env <- extendEnv tv (mapM tycheckC cl >> ask)
        constraints <- gets cs
        trace (show constraints ++ show env) $
            return (env `subst` constraints)

subst [] _ = []
subst ((v,t):rest) constraints = (v, substT t):(subst rest constraints)
    where substT (TyFun t t') = TyFun (substT t) (substT t')
          substT (TyVar tv) =
                case lookup tv constraints of
                    Just t -> substT t
                    Nothing -> TyVar tv
          substT (TyTup tl) = TyTup (map substT tl)
          substT t = t

tycheckC c@(HpClaus h b) = do
    let vars = getBoundedVars c
    ts <- sequence (replicate (length vars) (newTyVar>>=generalize))
    extendEnv (zip vars ts) (do { tycheckA (unLoc h); tycheckAL b} )

tycheckA (HpAtom e) = do
    tycheckE (unLoc e) (TyCon TyBool)

tycheckA (HpCut) = return ()

tycheckAL [] = return ()
tycheckAL (a:as) = do
    tycheckA (unLoc a)
    tycheckAL as

tyinferE expr = do
    exp_ty <- newTyVar
    tycheckE expr exp_ty
    return exp_ty

tycheckE (HpTerm t) exp_ty =
    tycheckT (unLoc t) exp_ty

tycheckE (HpPar e) exp_ty =
    tycheckE (unLoc e) exp_ty

tycheckE (HpPred p) exp_ty =
    tycheckT (HpVar p) exp_ty

tycheckE (HpTyExp e ann_ty) exp_ty = do
    ann_ty' <- instantiate ann_ty
    tycheckE (unLoc e) ann_ty'
    tyunify ann_ty' exp_ty
    return ()

tycheckE (HpApp exp exps) exp_ty = do
    fun_ty <- tyinferE (unLoc exp)
    (arg_ty, res_ty) <- tyunifyFun fun_ty
    tup_ty <- tyunifyL (length exps) arg_ty
    -- tycheckE exps arg_ty
    mapM_ (\(e,t) -> tycheckE (unLoc e) t) (zip exps tup_ty)
    tyunify res_ty exp_ty

tycheckT (HpCon c)    exp_ty = tyunify (TyCon TyAll) exp_ty
tycheckT (HpVar v)    exp_ty = do
    var_ty <- lookupVar v >>= instantiate
    tyunify var_ty exp_ty

tycheckT (HpId v) exp_ty = tycheckT (HpVar v) exp_ty

tycheckT (HpFun _ tl) exp_ty = do
    mapM_ (\(e,t) -> tycheckT (unLoc e) t) (zip tl (repeat (TyCon TyAll)))
    tyunify (TyCon TyAll) exp_ty

tycheckT (HpTup tl) exp_ty = do
    mapM_ (\(e,t) -> tycheckT (unLoc e) t) (zip tl (repeat (TyCon TyAll)))
    tyunify (TyCon TyAll) exp_ty

tycheckT (HpList tl maybe_t) exp_ty = do
    mapM_ (\(e,t) -> tycheckT (unLoc e) t) (zip tl (repeat (TyCon TyAll)))
    case maybe_t of
        Just t -> tycheckT (unLoc t) (TyCon TyAll)
        Nothing -> return ()
    tyunify (TyCon TyAll) exp_ty

tycheckT (HpWild) exp_ty = return ()

tyunify :: MonoType -> MonoType -> Tc ()
tyunify (TyFun arg1 res1) (TyFun arg2 res2) = do
    tyunify arg1 arg2
    tyunify res1 res2

tyunify (TyTup tl) (TyTup tl') = do
    check (length tl == length tl') $ (TypeError (text ""))
    mapM_ (\(t,t') -> tyunify t t') (zip tl tl')

tyunify (TyVar v) (TyVar v')
    | v == v' = return ()

tyunify (TyVar v) t = tyunifyVar v t
tyunify t (TyVar v) = tyunifyVar v t

tyunify t t' = 
    check (t == t') (TypeError (sep [ text "Cannot Unify types:", ppr t, text "and", ppr t']))

tyunifyVar v t = do
    maybe_ty <- lookupTyVar v
    case maybe_ty of
        Nothing -> tyunifyUVar v t
        Just t' -> tyunify t' t

tyunifyUVar v1 t@(TyVar v2) = do
    maybe_ty <- lookupTyVar v2
    case maybe_ty of
        Nothing -> addConstraint v1 t
        Just ty -> tyunify (TyVar v1) ty

tyunifyUVar v1 ty = do
    tvs <- getTyTvs ty
    check (v1 `notElem` tvs) (TypeError (text "occurcheck"))
    addConstraint v1 ty

getTyTvs (TyVar v) = do
    maybe_ty <- lookupTyVar v
    case maybe_ty of
        Nothing -> return [v]
        Just ty' -> getTyTvs ty' >>= \tvs' -> return (v:tvs')

getTyTvs (TyFun ty1 ty2) = do
    tvs1 <- getTyTvs ty1
    tvs2 <- getTyTvs ty2
    return (tvs1 ++ tvs2)

getTyTvs (TyTup tl) = do
    l <- mapM getTyTvs tl
    return (concat l)

getTyTvs (TyCon _) = return []

tyunifyFun (TyFun arg_ty res_ty) = return (arg_ty, res_ty)
tyunifyFun ty = do
    arg_ty <- newTyVar
    res_ty <- newTyVar
    tyunify ty (TyFun arg_ty res_ty)
    return (arg_ty, res_ty)

tyunifyL m t@(TyTup tl) = do
    check (m == length tl) (TypeError (text "expected type:" <+> ppr t))
    return tl

tyunifyL 1 t = do
    t' <- newTyVar
    tyunify t' t
    return [t']

tyunifyL m t = do
    tl <- sequence (replicate m newTyVar)
    tyunify (TyTup tl) t
    return tl
