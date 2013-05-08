module Basic where

-- A class describing a data structure that can be flattened
-- into a list
class Flatable f where 
    flatten :: f -> [f]

-- Class for types with arity, inplemented as a partial
-- function
class HasArity a where 
    arity :: a -> Maybe Int

-- Types with a descriptive name
class HasName a where
    nameOf :: a -> String
    
