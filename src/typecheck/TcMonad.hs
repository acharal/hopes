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
        l      :: Loc,
        msgs   :: Messages,
        constr :: ConstrEnv
    }


type TypeEnv = [ (HpSymbol, Type) ]
-- type TypeEnv a = Map.Map a Type

type Constraint = (TyVar, MonoType)

type ConstrEnv = [ (TyVar, MonoType) ]
-- type ConstrEnv = Map.Map TyVar MonoType

type Tc = ReaderT TcEnv (StateT TcState (ErrorT Messages Identity))

instance MonadLoc Tc where
    getLoc = gets l

instance MonadSetLoc Tc where
    withLoc l' m = modify (\x -> x{l = l'}) >> m

runTc m = 
    case run of
        Left msgs -> (Nothing, msgs)
        Right (a, st) -> (Just a, msgs st)
    where run = runIdentity $ runErrorT $ runStateT (runReaderT m initEnv) initSt
          initSt  = TcState { l = bogusLoc, uniq = 0, msgs = emptyMsgs, constr = [] }
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
    l <- getLoc
    diagnosis <- asks ctxt
    throwError $ mkMsgs $ mkErrWithLoc l TypeError Failure err diagnosis

newTyVar :: Tc MonoType
newTyVar = do
    n <- gets uniq
    modify (\s -> s{uniq = n+1})
    return (TyVar (n+1))

extendEnv :: [(HpSymbol, Type)] -> Tc a -> Tc a
extendEnv binds m = local extend m
    where extend env = env{tyenv = binds ++ (tyenv env)}

lookupVar :: HpSymbol -> Tc Type
lookupVar v = do
    ty_env <- asks tyenv
    case lookup v ty_env of
        Nothing -> typeError (sep [text "Variable", quotes (ppr v), text "out of scope"] )
        Just ty -> return ty

lookupTyVar :: TyVar -> Tc (Maybe MonoType)
lookupTyVar tv = do
    cs <- gets constr
    return (lookup tv cs)

addConstraint :: TyVar -> MonoType -> Tc ()
addConstraint tv ty =
    modify (\s -> s{constr = (tv,ty):(constr s)})

enterContext :: Context -> Tc a -> Tc a
enterContext c m = local addctxt m
    where addctxt env = env{ctxt = c:(ctxt env)}

{-
withContext :: Context -> Tc a -> Tc a
withContext c m = withLoc (loc c) $ local (\e -> e{ctxt = c:(ctxt env)) m

clauseCtxt lcl@(L loc cl) = 
    hang (if fact lcl then text "In fact:" else text "In rule:") 4 (ppr cl)
atomCtxt (L loc atom) = hang (text "In atom:") 4 (ppr atom)
exprCtxt (L loc expr) = hang (text "In expr:") 4 (ppr expr)

data Context = 
      CtxtClause LHpClause
    | CtxtAtom LHpAtom 
    | CtxtExpr LHpExpr


instance HasLocation Context where
    locSpan (CtxtClause x)  = locSpan x
    locSpan (CtxtAtom x)    = locSpan x
    locSpan (CtxtExpr x)    = locSpan x

-}