--  Copyright (C) 2007 2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Symbol where

import Types

data Symbol a = Sym a | AnonSym

instance Eq a => Eq (Symbol a) where
    (Sym a) == (Sym b) = a == b
    _ == _ = False

instance Show a => Show (Symbol a) where
    showsPrec p (Sym a) = showsPrec p a
    showsPrec p AnonSym = showString "_"

liftSym :: a -> Symbol a
liftSym a = Sym a


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

symbolsSig :: Sig s -> [s]
symbolsSig (as, bs) = as ++ bs
varsSig    (as, bs) = bs
rigidsSig  (as, bs) = as

symbols :: HasSignature a b => a -> [b]
vars    :: HasSignature a b => a -> [b]
rigids  :: HasSignature a b => a -> [b]

symbols = symbolsSig . sig
vars    = varsSig    . sig
rigids  = rigidsSig  . sig

{- 
specialSyms = 
 [ Special ":"  (TyFun (TyTup [tyAll, tyAll]) tyAll)
 , Special "[]" (tyAll)
 , Special "!"  (tyBool)
 , Special "s"  (TyFun tyAll tyAll)
 , Special "0"  (tyAll)
 ]
-}

buildinSym = [  liftSym ":",
                liftSym "[]",
                liftSym "!",
                liftSym "s",
                liftSym "0" ]

buildinTyp (Sym ":")  = TyFun (TyTup [tyAll, tyAll]) tyAll
buildinTyp (Sym "[]") = tyAll
buildinTyp (Sym "!" ) = tyBool
buildinTyp (Sym "s" ) = TyFun tyAll tyAll
buildinTyp (Sym "0" ) = tyAll
