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

module Hopl where

import Pretty
import Symbol
import Types
import Data.Monoid

data Expr a = 
      App (Expr a) (Expr a)
    | Set [Expr a] [a]
    | Flex a 
    | Rigid a 
    | Tup [Expr a]
    deriving Eq

type Clause a = (Expr a, [Expr a])

type Goal a = [Expr a]

type Prog a = [Clause a]

contradiction = []

isContra []    = True
isContra (_:_) = False

instance HasSignature (Expr a) a where
	sig (App e1 e2) = sig e1 `mappend` sig e2
	sig (Set _ _) = emptySig
	sig (Flex a)  = varSig a
	sig (Rigid a) = rigSig a
	sig (Tup es ) = mconcat $ map sig es

instance HasSignature (Clause a) a where
	sig (e1, e2) = sig e1 `mappend` sig e2

instance HasSignature (Goal a) a where
	sig gs = mconcat $ map sig gs

instance HasSignature (Prog a) a where
	sig cs = mconcat $ map sig cs


instance HasType a => HasType (Expr a) where
	typeOf (App e1 e2) = b
		where (TyFun a b) = typeOf e1
	typeOf (Set _ _) = error ""
	typeOf (Flex a)  = typeOf a
	typeOf (Rigid a) = typeOf a
	typeOf (Tup tys) = TyTup $ map typeOf tys

instance HasType a => HasType (Clause a) where
	typeOf c = liftGround TyBool

instance HasType a => HasType (Goal a) where
	typeOf g = liftGround TyBool

liftSet :: Expr a -> Expr a
liftSet e@(Set _ _) = e
liftSet (Flex v) = Set [] [v]
liftSet _ = error ("Cannot represent expression as set")


instance Pretty a => Pretty (Expr a) where
    ppr (Flex  sym)        = ppr sym
    ppr (Rigid sym)        = ppr sym
    ppr (Set es vs)        = curly  $ sep $ (punctuate comma (map ppr es)) ++ map (\v -> text "|" <+> ppr v) vs
    ppr (Tup es)           = parens $ sep $ (punctuate comma (map ppr es))
    ppr (App e e'@(Tup _)) = ppr e <> ppr e'
    ppr (App e e')         = ppr e <> parens (ppr e')

instance Pretty a => Pretty (Clause a) where
    ppr (h,b) = hang (sep [ppr h, entails]) 4 (sep (punctuate comma (map ppr  b)))

instance Pretty a => Pretty (Goal a) where
    ppr g = text "-?" <+> sep (punctuate comma (map ppr g))


instance Pretty a => Pretty (Prog a) where
    ppr p = vcat $ map ppr p
