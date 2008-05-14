--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

-- | Symbol module contains the definitions of a symbol,
--   either constant or variable
module Symbol where

{-

import Data.Monoid

class ClassSymbol a where
    toString :: a -> String
    isVariable :: a -> Bool
    isRigid :: a -> Bool

data Symbol =
      Rigid String
    | Var (Maybe Int)
    | Buildin String

newtype Signature a = Sig [a]

class Monoid Signature where
    mempty  = Sig mempty
    mappend (Sig x) (Sig y) = Sig $ mappend x y

class ClassSymbol s => HasSignature a s where
    sig :: a -> Signature s


symbolsOf :: (ClassSymbol s, HasSignature a s) => a -> [s]
varsOf    :: (ClassSymbol s, HasSignature a s) => a -> [s]
rigidsOf  :: (ClassSymbol s, HasSignature a s) => a -> [s]


Symbol information;
    - if its rigid or variable
    - type
    - arity
    - module name
-}

type Name = String

data Symbol a = Sym a | AnonSym

data Var =
      V Name
    | NoNameVar

instance Show Var where
    showsPrec p (V n) = showsPrec p n
    showsPrec p (NoNameVar) = showString "_"

instance Eq a => Eq (Symbol a) where
    (Sym a) == (Sym b) = a == b
    _ == _ = False

instance Show a => Show (Symbol a) where
    showsPrec p (Sym a) = showsPrec p a
    showsPrec p AnonSym = showString "_"

liftSym :: a -> Symbol a
liftSym a = Sym a

-- | Signature is the union of constants and variables
type Sig a = ([a], [a])

emptySig = ([],[])
joinSig (a1,a2) (b1,b2) = (a1 ++ b1, a2 ++ b2)
rigSig s = ([s], [])
varSig s = ([], [s])

class HasSignature a s where
    sig :: a -> Sig s

instance HasSignature (Symbol a) (Symbol a) where
    sig (Sym s) = rigSig (Sym s)
    sig AnonSym = emptySig

class HasVariables a where
    vars' :: a -> [Var]

vars    :: HasSignature a b => a -> [b]
rigids  :: HasSignature a b => a -> [b]

-- symbols :: HasSignature a b => a -> [b]
-- symbols = symbolsSig . sig where symbolsSig (as, bs) = as ++ bs
vars    = varsSig    . sig where varsSig    (as, bs) = bs
rigids  = rigidsSig  . sig where rigidsSig  (as, bs) = as
