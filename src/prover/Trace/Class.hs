{-# LANGUAGE
    FlexibleInstances
   ,FunctionalDependencies
   ,MultiParamTypeClasses
   ,UndecidableInstances
#-}

module Trace.Class where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Logic

class Monad m => MonadTrace a m | m -> a where
    trace :: a -> m ()

-- trace is ignored
-- instance MonadTrace a (Identity) where
--    trace a = return ()

instance MonadTrace a m => MonadTrace a (ReaderT e m) where
    trace a = lift (trace a)

instance MonadTrace a m => MonadTrace a (StateT s m) where
    trace a = lift (trace a)

instance MonadTrace a m => MonadTrace a (LogicT m) where
    trace a = lift (trace a)



traceResult m = traceMM return m

traceM f m = traceMM (return . f) m

traceMM f m = do
    a <- m
    b <- f a
    trace b
    return a
