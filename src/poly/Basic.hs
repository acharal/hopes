module Basic where

-- Symbol is an alias for the string type 
type Symbol = String

-- A class describing a data structure that can be flattened
-- into a list
class Flatable f where 
    flatten :: f a -> [f a]

instance Flatable [] where 
    flatten = map (\a -> [a])

-- Class for types with arity, inplemented as a partial
-- function
class HasArity a where 
    arity :: a -> Maybe Int

-- Types with a descriptive name
class HasName a where
    nameOf :: a -> Symbol
    


-- The reverse application operator. Thanks F#!

infixl 0 |>

x |> f = f x
