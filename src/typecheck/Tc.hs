module Tc where

import Control.Monad.Reader
import Control.Monad.State

import Err
import Loc

import Syntax
import Types
import Pretty
import Data.IORef


data TcEnv =
    TcEnv { 
        tyenv :: TypeEnv,
        ctxt  :: [Context HpSymbol]
    }

data TcState = 
    TcState {
        uniq   :: Int,
        msgs   :: Messages
    }

type TypeEnv = [ HpTySign ]

emptyEnv = []

instance Pretty TypeEnv where
    ppr ts = vcat $ map (\(v, t) -> sep [ ppr v, text "::", ppr t] ) ts

type Tc = ReaderT TcEnv (StateT TcState (ErrorT Messages IO))

instance MonadLoc Tc where
    getLoc = asks ctxt >>= return . loc

runTc m =  run >>= \res -> case res of
                                (Left msg) -> return (Nothing, msg)
                                (Right (p, s)) -> return (Just p, msgs s)
    where run = runErrorT $ runStateT (runReaderT m initEnv) initSt
          initSt  = TcState { uniq = 0, msgs = emptyMsgs }
          initEnv = TcEnv { tyenv = [], ctxt  = [] }

runTcWithEnv env m = runTc (extendEnv env m)

recoverTc :: Tc a -> Tc a -> Tc a
recoverTc main recov = 
    catchError main (\msgs -> addMsgs msgs >> recov)

addMsgs m = modify (\s -> s{ msgs = concatMsgs m (msgs s) })

getTypeEnv  :: Tc TypeEnv
getTypeEnv = asks tyenv

extendEnv :: [HpTySign] -> Tc a -> Tc a
extendEnv binds m = local extend m
    where extend env = env{tyenv = binds ++ (tyenv env)}

withTypeEnv = extendEnv

lookupVar :: HpSymbol -> Tc Type
lookupVar v = do
    ty_env <- asks tyenv
    case lookup v ty_env of
        Nothing -> typeError (sep [text "Variable", quotes (ppr v), text "out of scope"] )
        Just ty -> return ty

freshTyVar :: Tc MonoType
freshTyVar = do
    n <- gets uniq
    modify (\s -> s{uniq = n+1})
    r <- liftIO $ newIORef Nothing
    return (TyVar (Tv (n+1) r))

lookupTy :: TyVar -> Tc (Maybe MonoType)
lookupTy (Tv i r) = liftIO $ readIORef r

addConstraint :: TyVar -> MonoType -> Tc ()
addConstraint (Tv i r) ty = do
    maybet <- liftIO $ readIORef r
    case maybet of 
        Just ty' -> typeError (sep [text "tyvar", quotes (int i), text "already bind with type", ppr ty'])
        Nothing ->  liftIO $ writeIORef r (Just ty)


normEnv = 
    let aux (v,t) = do
            t' <- normType t
            return (v, t')
    in  getTypeEnv >>= mapM aux

normType (TyFun t1 t2) = do
    t1' <- normType t1
    t2' <- normType t2
    return $ TyFun t1' t2'

normType tvy@(TyVar tv) = do
    ty <- lookupTy tv
    case ty of
        Just t  -> do 
            ty' <- normType t
            let (Tv _ r) = tv
            liftIO $ writeIORef r (Just ty')
            return ty'
        Nothing -> return tvy

normType (TyTup tl) = do
    tl' <- mapM normType tl
    return $ TyTup tl'

normType t = return t

-- typeError :: Desc -> Tc ()
typeError :: ErrDesc -> Tc a
typeError err = do
    l <- getLoc
    diagnosis <- asks ctxt
    throwError $ mkMsgs $ mkErrWithLoc l TypeError Failure (vcat [err, vcat (map ppr diagnosis)])


enterContext c m = local addctxt m
    where addctxt env = env{ctxt = c:(ctxt env)}


data Context a = 
      CtxtExpr (LHpExpr a)
    | CtxtAtom (LHpExpr a)
    | CtxtForm (LHpFormula a)

instance HasLocation (Context a) where
    locSpan (CtxtExpr a) = locSpan a
    locSpan (CtxtForm a) = locSpan a
    locSpan (CtxtAtom a) = locSpan a

instance Pretty a => Pretty (Context a) where
    ppr (CtxtExpr a) = hang (text "In expr:") 4 (ppr (unLoc a))
    ppr (CtxtForm a) = hang (if isFact a then text "In fact:" else text "In rule:") 4 (ppr (unLoc a))
    ppr (CtxtAtom a) = hang (text "In atom:") 4 (ppr (unLoc a))


