--  Copyright (C) 2013 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--                     Emmanouil Koukoutos   <manoskouk@softlab.ntua.gr>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.


-- Operator and operator table implementation

module Operator where

import Data.List

data Operator = Operator { opName :: String,  opAssoc :: String }
   deriving (Eq, Show)

type OperatorTable = [(Int, Operator)]


precOp = fst

updateOpTable table op = op : deleteBy eqOp op table
   where eqOp (_, op1) (_, op2) = op1 == op2

keyGroup :: (Eq a, Eq b) => [(a, b)] -> [(a, [b])]
keyGroup lst = map aux $ group lst
    where aux lst = (fst (head lst), map snd lst)

groupByPrec :: OperatorTable -> [(Int, [Operator])]
groupByPrec table = map toCompact $
                        groupBy (\x y-> precOp x == precOp y) $
                        sortBy precComp table
   where precComp op1 op2  = compare (precOp op1) (precOp op2)
         toCompact x = (fst $ head x, map snd x)

-- | determines fixity by the place of "f"
isPrefixOp   (Operator _ s) = length s == 2 && head s == 'f'
isPostfixOp  (Operator _ s) = length s == 2 && last s == 'f'
isInfixOp    (Operator _ s) = length s == 3 && head (tail s) == 'f'

-- | determines associativity by the place of "x" (meaning terms of same precedence)
-- | in case of yfy there is no assosiativity
isAssocRight op@(Operator _ s) = isInfixOp op && last s == 'x'
isAssocLeft  op@(Operator _ s) = isInfixOp op && not (isAssocRight op) && head s == 'x'
isAssocNone  op@(Operator _ s) = not (isAssocRight op) && not (isAssocLeft op)
