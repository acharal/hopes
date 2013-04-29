module Language.Prolog.Operator where

import Data.List

data Operator = Operator { opName :: String,  opAssoc :: String }
   deriving (Eq, Show)

type OperatorTable = [(Int, Operator)]


precOp (prec, op) = prec

updateOpTable table op = op : deleteBy eqOp op table
   where eqOp (_, op1) (_, op2) = op1 == op2

keyGroup :: (Eq a, Eq b) => [(a, b)] -> [(a, [b])]
keyGroup lst = map aux $ group lst
    where aux lst = (fst (head lst), map snd lst)

groupByPrec :: OperatorTable -> [(Int, [Operator])]
groupByPrec table = keyGroup $ sortBy precComp table
   where precComp op1 op2  = compare (precOp op1) (precOp op2)


-- | determines fixity by the place of "f"
isPrefixOp   (Operator _ s) = length s == 2 && head s == 'f'
isPostfixOp  (Operator _ s) = length s == 2 && last s == 'f'
isInfixOp    (Operator _ s) = length s == 3 && head (tail s) == 'f'

-- | determines associativity by the place of "x" (meaning terms of same precedence)
-- | in case of yfy there is no assosiativity
isAssocRight op@(Operator _ s) = isInfixOp op && last s == 'x'
isAssocLeft  op@(Operator _ s) = isInfixOp op && not (isAssocRight op) && head s == 'x'
isAssocNone  op@(Operator _ s) = not (isAssocRight op) && not (isAssocLeft op)


