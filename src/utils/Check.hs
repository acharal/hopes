module Check where

import Control.Monad.Error

class MonadCheck e m where
    check :: Bool -> e -> m ()

instance (Monad m, Error e) => MonadCheck e (ErrorT e m) where
    check True e  = return ()
    check False e = throwError e