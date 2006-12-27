module CheckT where

import Control.Monad.Error
import Err

-- for the time being CheckT can only fail once

type CheckT = ErrorT 

class MonadCheck e m where
    check :: Bool -> e -> m ()

instance (Error e, Monad m) => MonadCheck e (CheckT e m) where
    check False err = throwError err
    check True  _   = return ()
