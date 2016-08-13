

module Trace.Coroutine where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Applicative (Alternative(..))
import Logic.Class
import Trace.Class
import Infer.Class

type TraceT i m  = Coroutine (Yield i) m


--instance (Functor s, Monad m) => Functor (Coroutine s m) where
--    fmap = liftM


instance Monad m => MonadTrace a (Coroutine (Yield a) m) where
    trace a = yield a

instance (Functor s, MonadPlus m) => MonadPlus (Coroutine s m) where
    mzero = Coroutine mzero
    m1 `mplus` m2 = Coroutine (resume m1 `mplus` resume m2)

instance (Functor s, Functor m, MonadPlus m) => Alternative (Coroutine s m) where
    empty = mzero
    (<|>) = mplus

instance (Functor s, MonadLogic m) => MonadLogic (Coroutine s m) where
    msplit m = Coroutine (msplit (resume m) >>= apply)
        where apply (Nothing)    = return $ Right Nothing
              apply (Just (Right a, s)) = return $ Right $ Just (a, Coroutine s)
              apply (Just (Left a, s))  = return $ Left  $
                fmap (\x -> x >>= \y -> return (Just (y, Coroutine s))) a

instance (Monad m, Functor s, MonadFreeVarProvider m) => MonadFreeVarProvider (Coroutine s m) where
    freshVarOfType = lift . freshVarOfType
    freshVarFrom = lift . freshVarFrom

instance (Monad m, Functor s, MonadClauseProvider m) => MonadClauseProvider (Coroutine s m) where
    clausesOf = lift . clausesOf

instance (Functor ss, MonadState s m) => MonadState s (Coroutine ss m) where
    get = lift $ get
    put = lift . put

runTraceT m h = pogoStick (f h) m
    where f h (Yield i c) = h i c
