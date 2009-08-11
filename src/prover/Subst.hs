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

module Subst where

import Hopl
-- import Data.Monoid (mconcat)

type Subst a = [ (a, Expr a) ]

success :: Subst a
success = []

bind :: a -> Expr a -> Subst a
bind v e = [(v, e)]


isTaut :: Eq a => (a, Expr a) -> Bool
isTaut (v, Flex v') = v == v'
isTaut _ = False


class (Substitutable a b) | a -> b where
	subst :: Substitutable a b => (Subst b) -> a -> a

instance Eq a => (Substitutable (Expr a) a) where
        subst theta (App e1 e2) = App (subst theta e1) (subst theta e2)
        subst theta (Lambda x e) =
            case lookup x theta of
                Nothing -> Lambda x (subst theta e)
                Just e' -> case e' of
                            Flex y -> (Lambda y (subst theta e))
                            _ -> error "Cannot substitute lambda bound vars with expr"
        subst theta e@(Flex x) =
            case lookup x theta of
                Nothing -> e
                Just e' -> e'
        subst theta e = e

instance Eq a => (Substitutable (Clause a) a) where
	subst theta (C h b) = (C h (subst theta b))


instance Eq a => (Substitutable (Subst a) a) where
	subst theta zeta = [ (v, e') | (v, e) <- theta, let e' = subst zeta e, not (isTaut (v, e')) ]


restrict :: Eq a => [a] -> Subst a -> Subst a
restrict ss xs = [ (v, e) | (v, e) <- xs, v `elem` ss ]

combine theta zeta  = 
    let ss    = map fst theta
        zeta' = [ (v, e) | (v, e) <- zeta, v `notElem` ss ]
    in  subst theta zeta ++ zeta'

