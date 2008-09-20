--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

-- | type checker monad
module Tc where

import Control.Monad.Reader
import Control.Monad.State

import Loc
import Error

import Syntax
import Lang
import Types
import Pretty
import Data.IORef


data TcEnv =
    TcEnv { 
        tcTyEnv :: TyEnv HpSymbol,
        ctxt  :: [Context]
    }

data TcState = 
    TcState {
        uniq   :: Int,
        msgs   :: Messages
    }


type Tc = ReaderT TcEnv (StateT TcState (ErrorT Messages IO))

instance MonadLoc Tc where
    getLoc = asks ctxt >>= return . loc

runTc m =  run >>= \res -> case res of
                                (Left msg) -> return (Nothing, msg)
                                (Right (p, s)) -> return (Just p, msgs s)
    where run = runErrorT $ runStateT (runReaderT m initEnv) initSt
          initSt  = TcState { uniq = 0, msgs = emptyMsgs }
          initEnv = TcEnv   { tcTyEnv = [], ctxt  = [] }

runTcWithEnv env m = runTc (extendEnv env m)

recoverTc :: Tc a -> Tc a -> Tc a
recoverTc main recov = 
    catchError main (\msgs -> addMsgs msgs >> recov)

addMsgs m = modify (\s -> s{ msgs = concatMsgs m (msgs s) })

getTypeEnv  :: Tc (TyEnv HpSymbol)
getTypeEnv = asks tcTyEnv

extendEnv :: [TySig HpSymbol] -> Tc a -> Tc a
extendEnv binds m = local extend m
    where extend env = env{tcTyEnv = binds ++ (tcTyEnv env)}

withTypeEnv = extendEnv

lookupVar :: HpSymbol -> Tc Type
lookupVar v = do
    ty_env <- asks tcTyEnv
    case lookupTyEnv v ty_env of
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

-- normType (TyTup tl) = do
--    tl' <- mapM normType tl
--    return $ TyTup tl'

normType t = return t

-- typeError :: Desc -> Tc ()
typeError :: ErrDesc -> Tc a
typeError err = do
    l <- getLoc
    diagnosis <- asks ctxt
    throwError $ mkMsgs $ mkErrWithLoc l TypeError Failure (vcat [err, vcat (map ppr diagnosis)])


instantiate :: Type -> Tc MonoType
instantiate t = return t

generalize :: MonoType -> Tc Type
generalize t = return t

freshTyFor :: a -> Tc (a, Type)
freshTyFor v = do
    ty <- freshTyVar >>= generalize
    return (v, ty)

withSig e m = do
	ty_sym <- mapM freshTyFor (rigids e)
	withTypeEnv ty_sym m

enterContext c m = local addctxt m
    where addctxt env = env{ctxt = c:(ctxt env)}


data Context = 
      CtxtExpr (LHpExpr   HpSymbol)
    | CtxtAtom (LHpExpr   HpSymbol)
    | CtxtForm (LHpClause HpSymbol)

instance HasLocation Context where
    locSpan (CtxtExpr a) = locSpan a
    locSpan (CtxtForm a) = locSpan a
    locSpan (CtxtAtom a) = locSpan a

instance Pretty Context where
    ppr (CtxtExpr a) = hang (text "In expr:") 4 (ppr (unLoc a))
    ppr (CtxtForm a) = hang (if isFact a then text "In fact:" else text "In rule:") 4 (ppr (unLoc a))
    ppr (CtxtAtom a) = hang (text "In atom:") 4 (ppr (unLoc a))


