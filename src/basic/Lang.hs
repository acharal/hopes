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

{-# LANGUAGE MultiParamTypeClasses #-}

module Lang where

import Types (Typed(..))

type Name = String

class Symbol a where
    liftSym :: String -> a

data Sym = Sym String | AnonSym

data Var =
      V Name
    | NoNameVar

instance Show Var where
    showsPrec p (V n) = showsPrec p n
    showsPrec p (NoNameVar) = showString "_"

instance  Show Sym where
    showsPrec p (Sym a) = showsPrec p a
    showsPrec p AnonSym = showString "_"

instance Symbol Sym where
    liftSym a = Sym a

instance Eq Sym where
    (Sym s1) == (Sym s2) = s1 == s2
    AnonSym == s = False
    s == AnonSym = False

instance Symbol a => Symbol (Typed a) where
    liftSym a = T (liftSym a) (error "not valid type")

-- | Signature is the union of constants and variables
type Sig a = ([a], [a])

emptySig = ([],[])
joinSig (a1,a2) (b1,b2) = (a1 ++ b1, a2 ++ b2)
rigSig s = ([s], [])
varSig s = ([], [s])

class Symbol s => HasSignature a s where
    sig :: a -> Sig s

{-
instance Symbol a => HasSignature a a where
    sig a = rigSig a
    -- sig AnonSym = emptySig
-}

instance HasSignature Sym Sym where
    sig AnonSym = emptySig
    sig a = rigSig a

vars    :: (Symbol b, HasSignature a b) => a -> [b]
rigids  :: (Symbol b, HasSignature a b) => a -> [b]

-- symbols :: HasSignature a b => a -> [b]
-- symbols = symbolsSig . sig where symbolsSig (as, bs) = as ++ bs
vars    = varsSig    . sig where varsSig    (as, bs) = bs
rigids  = rigidsSig  . sig where rigidsSig  (as, bs) = as

class Eq a => HasLogicConstants a where
    ctop     :: a
    cbot     :: a
    cor      :: a
    cand     :: a
    ceq      :: a
    cexists  :: a


contradiction :: HasLogicConstants a => a
contradiction = ctop

isContra :: HasLogicConstants a => a -> Bool
isContra = (==contradiction)

