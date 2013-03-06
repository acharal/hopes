
module Trace (
    module Trace,
    module Trace.Class,
    module Trace.Coroutine
) where

import Control.Monad
import Control.Monad.State


import Logic.Class
import Infer.Class
import Trace.Class

import Trace.Coroutine


data DebugState = 
    DebugState 
    { verbosity :: Int
    , outputTrace :: Bool
    , pauseEachStep :: Bool
    }


newtype DebugT i m a = DebugT { unDebugT :: TraceT i (StateT DebugState m) a }

instance Monad m => Monad (DebugT i m) where
    return a = DebugT $ return a
    m >>= f  = DebugT $ (unDebugT m) >>= \a -> unDebugT (f a)
    fail  = lift . fail

instance MonadTrans (DebugT i) where
    lift m = DebugT $ lift (lift m)

instance Monad m => MonadTrace a (DebugT a m)  where
    trace a = DebugT $ trace a

instance MonadPlus m => MonadPlus (DebugT a m) where
    mzero = DebugT $ mzero
    m1 `mplus` m2 = DebugT $ (unDebugT m1) `mplus` (unDebugT m2)

instance MonadLogic m => MonadLogic (DebugT a m) where
    msplit m = DebugT $ 
        msplit (unDebugT m) >>= \r -> 
            case r of 
                Nothing -> return Nothing
                Just (a, s) -> return $ Just (a, DebugT s)

instance (Monad m, MonadFreeVarProvider a m) => MonadFreeVarProvider a (DebugT b m) where
    freshVarOfType a = lift (freshVarOfType a)

instance (Monad m, MonadClauseProvider a m) => MonadClauseProvider a (DebugT b m) where
    clausesOf a = lift (clausesOf a)

instance MonadIO m => MonadIO (DebugT b m) where
    liftIO =  lift . liftIO

debug m h = runDebugT st m h
    where st = DebugState { verbosity = 0, outputTrace = True, pauseEachStep = True }

runDebugT st m h = evalStateT (runTraceT (unDebugT m) (f h)) st
    where f h i c = unDebugT (h i (DebugT c))

noDebugT m = runDebugT undefined m h
    where h _ c = c
