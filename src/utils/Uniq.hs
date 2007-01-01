module Uniq where

import Data.Unique
import Control.Monad.State

class Uniq a where
    hashUniq :: a -> Int

class MonadUniq a m where
    newUniq :: m a

instance MonadUniq Unique IO where
    newUniq = newUnique

instance Monad m => MonadUniq Int (StateT Int m) where
    newUniq = do
        u <- get
        modify (+1)
        return u