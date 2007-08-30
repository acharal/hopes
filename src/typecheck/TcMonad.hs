module TcMonad where


import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Err
import Loc

import Syntax
import Types
import Pretty
import Data.Monoid
import Data.IORef
-- import qualified Data.Map as Map

import Debug.Trace

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

type TypeEnv = [ (HpSymbol, Type) ]

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

failIfErr :: Tc ()
failIfErr = do
    m <- gets msgs
    when (hasErrs m) $ throwError m

extendEnv :: [(HpSymbol, Type)] -> Tc a -> Tc a
extendEnv binds m = local extend m
    where extend env = env{tyenv = binds ++ (tyenv env)}

lookupVar :: HpSymbol -> Tc Type
lookupVar v = do
    ty_env <- asks tyenv
    case lookup v ty_env of
        Nothing -> typeError (sep [text "Variable", quotes (ppr v), text "out of scope"] )
        Just ty -> return ty

newTyVar :: Tc MonoType
newTyVar = do
    n <- gets uniq
    modify (\s -> s{uniq = n+1})
    r <- liftIO $ newIORef Nothing
    return (TyVar (Tv (n+1) r))

lookupTyVar :: TyVar -> Tc (Maybe MonoType)
lookupTyVar (Tv i r) = liftIO $ readIORef r

addConstraint :: TyVar -> MonoType -> Tc ()
addConstraint (Tv i r) ty = do
    maybet <- liftIO $ readIORef r
    case maybet of 
        Just ty' -> typeError (sep [text "tyvar", quotes (int i), text "already bind with type", ppr ty'])
        Nothing ->  liftIO $ writeIORef r (Just ty)

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


