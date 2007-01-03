module TcMonad where


import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Err
import Loc

import HpSyn
import Types
import Pretty

data TcEnv =
    TcEnv { 
        tyenv :: TypeEnv,
        ctxt  :: [Context]
    }

data TcState = 
    TcState {
        uniq   :: Int,
        loc    :: Loc,
        msgs   :: Messages,
        constr :: [Constraint]
    }

type TypeEnv = [ (HpName, Type) ]
type Constraint = (TyVar, MonoType)

type Tc = ReaderT TcEnv (StateT TcState (ErrorT Messages Identity))

runTc m = 
    case run of
        Left msgs -> (Nothing, msgs)
        Right (a, st) -> (Just a, msgs st)
    where run = runIdentity $ runErrorT $ runStateT (runReaderT m initEnv) initSt
          initSt  = TcState { loc = noLoc, uniq = 0, msgs = emptyMsgs, constr = [] }
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

-- typeError :: Desc -> Tc ()
typeError :: ErrDesc -> Tc a
typeError err = do
    loc <- gets loc
    diagnosis <- asks ctxt
    throwError $ mkMsgs $ mkLocErr loc TypeError Failure err diagnosis

newTyVar :: Tc MonoType
newTyVar = do
    n <- gets uniq
    modify (\s -> s{uniq = n+1})
    return (TyVar (n+1))

extendEnv :: [(HpName, Type)] -> Tc a -> Tc a
extendEnv binds m = local extend m
    where extend env = env{tyenv = binds ++ (tyenv env)}

lookupVar :: HpName -> Tc Type
lookupVar v = do
    ty_env <- asks tyenv
    case lookup v ty_env of
        Nothing -> typeError (sep [text "Variable", quotes (text v), text "out of scope"] )
        Just ty -> return ty

lookupTyVar :: TyVar -> Tc (Maybe MonoType)
lookupTyVar tv = do
    cs <- gets constr
    return (lookup tv cs)

addConstraint :: TyVar -> MonoType -> Tc ()
addConstraint tv ty =
    modify (\s -> s{constr = (tv,ty):(constr s)})

inCtxt :: Context -> Tc a -> Tc a
inCtxt c m = local addctxt m 
    where addctxt env = env{ctxt = c:(ctxt env)}

setLoc :: Loc -> Tc ()
setLoc l = modify (\s -> s{loc=l})

wrapLoc :: Located a -> (Located a -> Tc b) -> Tc b
wrapLoc a@(L loc _) f = do
    setLoc (spanBegin loc)
    b <- f a 
    return b
