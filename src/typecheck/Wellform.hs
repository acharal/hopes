module Wellform where

{- checking well formated formulas -}
import Syntax
import Types
import Loc
import Tc
import Typecheck
import Restrict


-- well formatted program
wfp p = do
    (p', env)  <- tcProg p
    (p'', env) <- restrictProg (p', env)
    env <- zonkEnv env
    return (p'', env)

-- well formatted formula
wff f = do
    f' <- tcForm f
    restrictForm f'
    return f'

-- well formatted goal.
-- this is different from formula
-- because it may contain *new* symbols
wfg g = do
    g'  <- wff g
    env <- getTypeEnv
    env <- zonkEnv env
    return (g', env)

-- head must not contain "other than" higher order variables in higher-order arg positions
zonkEnv :: TypeEnv -> Tc TypeEnv
zonkEnv env = 
    let aux (v,t) = do
            t' <- zonkType t
            return (v, t')
    in  mapM aux env


zonkType (TyVar _) = return tyAll
zonkType (TyFun t1 t2) = do
    t1' <- zonkType t1
    t2' <- zonkType t2
    return (TyFun t1' t2')
zonkType (TyTup tl) = do
    tl' <- mapM zonkType tl
    return (TyTup tl')
zonkType t = return t

-- normProg :: HpProg a -> Tc (HpProg a)
normProg p = do 
    cl <- mapM normForm (clauses p)
    return (p { clauses = cl })

normForm (L l (HpForm b xs ys)) = do
    b' <- normBinds b
    xs' <- mapM normExpr xs
    ys' <- mapM normExpr ys
    return (L l (HpForm b' xs' ys'))

normBinds bs = mapM normBind bs
    where normBind (HpBind s ty) = do
            ty' <- normType ty
            s'   <- normSym s
            return (HpBind s' ty')

normExpr (L l (HpApp e es)) = do
    e'  <-  normExpr e
    es' <- mapM normExpr es
    return (L l (HpApp e' es'))
normExpr (L l (HpTup es)) = do
    es' <- mapM normExpr es
    return (L l (HpTup es'))
normExpr (L l (HpSym s)) = do
    s' <- normSym s
    return (L l (HpSym s'))

normSym  (TcS s i ty) = do
    ty' <- normType ty
    return (TcS s (arity ty) ty')
