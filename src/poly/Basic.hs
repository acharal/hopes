module Basic where

import Text.Parsec.Pos

-- Symbol is an alias for the string type 
type Symbol = String

-- A class describing a data structure that can be flattened
-- into a list
class Flatable a where 
    flatten :: a -> [a]

instance Flatable [a] where 
    flatten = map (\a -> [a])

-- Class for types with arity, inplemented as a partial
-- function
class HasArity a where 
    arity :: a -> Maybe Int

-- Types with a descriptive name
class HasName a where
    nameOf :: a -> Symbol
    


-- The reverse application/forward pipe operator. Thanks F#!

infixl 0 |>

x |> f = f x

-- Position spans
bogusPos = newPos "bogusFile" (-1) (-1) 
data PosSpan = PosSpan SourcePos SourcePos 
    deriving (Eq)
instance Show PosSpan where
    show p = "" -- TODO: show better

bogusSpan = PosSpan bogusPos bogusPos


