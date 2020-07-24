{-# LANGUAGE
    FlexibleInstances
   ,MultiParamTypeClasses
   ,NoMonomorphismRestriction
   ,UndecidableInstances
#-}

module Trace (
    module Trace,
    module Trace.Class,
    module Trace.Coroutine
) where

import Control.Monad
import Control.Monad.State
import Control.Applicative (Applicative(..), Alternative(..))


import Logic.Class
import Infer.Class
import Trace.Class

import Trace.Coroutine

newtype DebugT s i m a = DebugT { unDebugT :: TraceT i (StateT s m) a }

instance Monad m => Functor (DebugT s i m) where
    fmap = liftM

instance Monad m => Applicative (DebugT s i m) where
    pure a = DebugT $ return a
    (<*>) = ap

instance Monad m => Monad (DebugT s i m) where
    return a = pure a
    m >>= f  = DebugT $ (unDebugT m) >>= \a -> unDebugT (f a)
    fail  = lift . fail

instance MonadTrans (DebugT s i) where
    lift m = DebugT $ lift (lift m)

instance Monad m => MonadTrace a (DebugT s a m)  where
    trace a = DebugT $ trace a

instance MonadPlus m => Alternative (DebugT s a m) where
    empty = mzero
    (<|>) = mplus

instance MonadPlus m => MonadPlus (DebugT s a m) where
    mzero = DebugT $ mzero
    m1 `mplus` m2 = DebugT $ (unDebugT m1) `mplus` (unDebugT m2)

instance MonadLogic m => MonadLogic (DebugT s a m) where
    msplit m = DebugT $
        msplit (unDebugT m) >>= \r ->
            case r of
                Nothing -> return Nothing
                Just (a, s) -> return $ Just (a, DebugT s)

instance (Monad m, MonadFreeVarProvider a m) => MonadFreeVarProvider a (DebugT s b m) where
    freshVarOfType a = lift (freshVarOfType a)

instance (Monad m, MonadClauseProvider a m) => MonadClauseProvider a (DebugT s b m) where
    clausesOf a = lift (clausesOf a)

instance MonadIO m => MonadIO (DebugT s b m) where
    liftIO =  lift . liftIO

instance MonadState s m => MonadState s (DebugT s1 b m) where
    get = lift $ get
    put = lift . put

runDebugT st m h = evalStateT (runTraceT (unDebugT m) (f h)) st
    where f h i c = unDebugT (h i (DebugT c))

noDebugT m = runDebugT undefined m h
    where h _ c = c

getState = DebugT $ lift $ get
modifyState f = DebugT $ lift $ modify f
getsState f = getState >>= \s -> return (f s)

{-
instance MonadState s m => MonadState s (DebugT s b m) where
    get = DebugT $ lift $ get
    put s = DebugT $ lift (put s)
-}
