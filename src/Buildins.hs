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

module Buildins where

import Lang
import Types
import Syntax

consSym  = HpSym $ Sym "."
nilSym   = HpSym $ Sym "[]"
cutSym   = HpSym $ Sym "!"
succSym  = HpSym $ Sym "s"
zeroSym  = HpSym $ Sym "0"
wildcat  = HpSym $ AnonSym

buildinTyp (HpSym (Sym "."))  = TyFun tyAll (TyFun tyAll tyAll)
buildinTyp (HpSym (Sym "[]")) = tyAll
buildinTyp (HpSym (Sym "!" )) = tyBool
buildinTyp (HpSym (Sym "s" )) = TyFun tyAll tyAll
buildinTyp (HpSym (Sym "0" )) = tyAll
buildinTyp (HpSym AnonSym   ) = tyAll

mkBuildin s = HpSym $ Sym s

buildins = [
    consSym,
    nilSym,
    cutSym,
    succSym,
    zeroSym ]

--isBuildin :: Eq a => HpExpr a -> Bool
isBuildin s = any (s==) buildins

buildinsigs = zip (map (\(HpSym x) -> x) buildins) $ map buildinTyp buildins


{-
A buildin is a special predicate, that can be called from the prolog command
prompt, or more generally from a prolog file.
Special predicates are those that can't be expressed as clauses, and must be
directly implement from the interpreter.
Note that the use of that predicates may destroy the semantics of the language.

data Buildin =
    Buildin {
        name::(String),
        args::Int
        action:: HopeI () -- it's an action including IO and
                          -- program modification.
    }
-}
