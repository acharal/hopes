module Core where

import Hopl
import Syntax
import Types
import Tc (TypeEnv)
import Loc

import Control.Monad.Reader

import Pretty

instance Pretty a => Pretty (a, Type) where
    ppr (a,_) = ppr a


data CoreEnv = CEnv { rigty :: TypeEnv, bindings :: [HpBindings HpSymbol] }

type CoreTransfT = ReaderT CoreEnv

runCore m = runReaderT m (CEnv [] [])

ctProg :: Monad m => (HpProg HpSymbol, TypeEnv) -> CoreTransfT m (Prog (HpSymbol,Type))
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

ctExp (HpSym AnonSym) = return $ Flex (AnonSym, undefined)
ctExp (HpSym a)  = do
    te <- asks rigty
    le <- asks bindings
    case lookup a te of
        Nothing -> do
            case le of
                []    -> error ("wtf? no definition for variable "++ show a)
                (l:_) -> 
                    case lookupBind a l of
                        Nothing -> error "not implemented yet"
                        Just (HpBind v' ty) -> return $ Flex (v', ty)
        Just ty -> return $ Rigid (a,ty)


ctExp e = error "Expression must not occur in that phase"
