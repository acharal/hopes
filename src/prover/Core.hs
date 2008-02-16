module Core where

import Hopl
import Syntax
import Symbol
import Types
import Loc

import Control.Monad.Reader


data CoreEnv = CEnv { rigty :: TyEnv HpSymbol, bindings :: [HpBindings HpSymbol] }

type CoreTransfT = ReaderT CoreEnv

runCore m = runReaderT m (CEnv [] [])

ctProg :: Monad m => (HpProg HpSymbol, TyEnv HpSymbol) -> CoreTransfT m (Prog (Typed HpSymbol))
ctProg (p, ty_env) = local (\r -> r{ rigty = ty_env}) $ mapM (ctClause.unLoc) (clauses p)

ctGoal ((L _ (HpForm b [] ys)),ty_env) = 
    local (\r -> r{ rigty = ty_env, bindings = b:(bindings r)}) $ do
    bd <- mapM ctExp (map unLoc ys)
    return bd

ctClause (HpForm b [] ys)  = fail "Cannot transform clause without a head"
ctClause (HpForm b [x] ys) = 
    local (\r -> r {bindings = b:(bindings r)}) $ do
    h <- ctExp (unLoc x)
    b <- mapM ctExp (map unLoc ys)
    return (h, b)

ctExp (HpApp e es') = do
    ce   <- ctExp (unLoc e)
    ces' <- mapM ctExp (map unLoc es')
    ce'  <-
        case ces' of
            [x] -> return x
            xs -> return (Tup xs)
    return (App ce ce')

ctExp (HpTup es) = do
    ces <- mapM ctExp (map unLoc es)
    return (Tup ces)

ctExp (HpPar e)  = ctExp (unLoc e)

ctExp (HpSym AnonSym) = return $ Flex (typed tyAll AnonSym)
ctExp (HpSym a)  = 
    let grd (TyFun t1 t2) = TyFun (grd t1) (grd t2)
        grd (TyGrd c) = TyGrd c
        grd (TyVar _) = tyAll
        grd (TyTup ts) = TyTup (map grd ts)
    in do
    te <- asks rigty
    le <- asks bindings
    case lookup a te of
        Nothing -> do
            case le of
                []    -> error ("wtf? no definition for variable "++ show a)
                (l:_) -> 
                    case lookupBind a l of
                        Nothing -> error "not implemented yet"
                        Just (HpBind v' ty) -> return $ Flex (typed (grd ty) v')
        Just ty -> 
             case ty of 
                TyVar _ -> error (show a)
                _ -> return $ Rigid (typed ty a)


ctExp e = error "Expression must not occur in that phase"

