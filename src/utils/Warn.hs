module Warn where


class MonadWarn w m where
    addWarning :: w -> m a
