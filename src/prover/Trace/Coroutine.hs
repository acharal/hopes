

module Trace.Coroutine where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad

import Logic.Class
import Trace.Class


type TraceT i m  = Coroutine (Yield i) m



instance Monad m => MonadTrace a (Coroutine (Yield a) m) where
    trace a = yield a


instance (Functor s, MonadPlus m) => MonadPlus (Coroutine s m) where
    mzero = Coroutine mzero 
    m1 `mplus` m2 = Coroutine (resume m1 `mplus` resume m2)


instance (Functor s, MonadLogic m) => MonadLogic (Coroutine s m) where
    msplit m = Coroutine (msplit (resume m) >>= apply)
        where apply (Nothing)    = return $ Right Nothing
              apply (Just (Right a, s)) = return $ Right $ Just (a, Coroutine s)
              apply (Just (Left a, s))  = return $ Left  $ 
                fmap (\x -> x >>= \y -> return (Just (y, Coroutine s))) a

runTraceT m h = pogoStick (f h) m
    where f h (Yield i c) = h i c
