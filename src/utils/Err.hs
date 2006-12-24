module Err (
        ErrorT(..), throwError, catchError,
        HopeError(..)
    ) where

import Loc
import Ppr

import Control.Monad.Error
import Control.Monad.State

data HopeError = 
    ParseError Loc Message
  | Internal Message

instance Error HopeError where
    strMsg str = Internal (text str)


instance Pretty HopeError where
    ppr (Internal msg) = hang (text "Internal Error" <> colon) 4 msg
    ppr (ParseError l msg) = hang ((ppr l) <> colon) 4 msg

{-
class Monad m => MonadWarn w m where
   warn :: w -> m ()

type WarnT w = StateT [w]

runWarnT = runStateT

instance Monad m => MonadWarn w (WarnT w m) where
    warn w = modify (\s -> w:s)

instance MonadWarn w m => MonadWarn w (StateT s m) where
    warn = lift.warn

instance (Error e, MonadWarn w m) => MonadWarn w (ErrorT e m) where
    warn = lift.warn
-}
