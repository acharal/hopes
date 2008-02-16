module Bindings where

import Types

data Binding a = Bind (Typed a)

data Bindings a = [ Binding a ]

instance HasType (Binding a) where
    typeOf (Bind b) = typeOf b

symbolBind (Bind a) = unTyp a

class Eq b => HasBindings a b where
    binds  :: a -> Bindings b
    isBind :: a -> b -> Bool
    isBind a s = any (s==) $ map symbolBind (binds a)

lookupBind :: Eq a => a -> Binds a -> Maybe (Bind a)
lookupBind x = find ((x==).symbolBind)