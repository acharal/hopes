--  Copyright (C) 2006-2011 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

-- import Language.Hopl (Expr(..), Clause(..))
-- import Data.Monoid (mconcat)
import CoreLang
import Data.List (union)

type Subst a = [ (a, Expr a) ]

success :: Subst a
success = []

bind :: a -> Expr a -> Subst a
bind v e = [(v, e)]


isTaut :: Eq a => (a, Expr a) -> Bool
isTaut (v, Var v') = v == v'
isTaut _ = False

class (Substitutable a b) | a -> b where
	subst :: Substitutable a b => (Subst b) -> a -> a

instance Eq a => (Substitutable (Expr a) a) where
        subst theta (App e1 e2)  = App (subst theta e1) (subst theta e2)
        subst theta (And e1 e2)  = And (subst theta e1) (subst theta e2)
        subst theta (Or  e1 e2)  = Or  (subst theta e1) (subst theta e2)
        subst theta (Eq  e1 e2)  = Eq  (subst theta e1) (subst theta e2)
        subst theta (Lambda x e) = maybe (Lambda x (subst theta e)) aux $ lookup x theta
            where aux (Var y) = Lambda y (subst theta e)
                  aux _ = error "Cannot substitute lambda bounds vars with expr"
        subst theta (Exists x e) = maybe (Exists x (subst theta e)) aux $ lookup x theta
            where aux (Var y) = Exists y (subst theta e)
                  aux _ = error "Cannot substitute exist bounds vars with expr"
--        subst theta (Forall x e) = maybe (Forall x (subst theta e)) aux $ lookup x theta
--            where aux (Var y) = Forall y (subst theta e)
--                  aux _ = error "Cannot substitute lambda bounds vars with expr"
        subst theta e@(Var x) = maybe e id $ lookup x theta
        subst theta (Not e) = Not (subst theta e)
        subst theta (ListCons e1 e2) = ListCons (subst theta e1) (subst theta e2)
        subst theta (Select e) = Select (subst theta e)
        subst theta e = e

instance Eq a => (Substitutable (Subst a) a) where
	subst theta zeta = [ (v, e') | (v, e) <- theta, let e' = subst zeta e, not (isTaut (v, e')) ]


restrict :: Eq a => [a] -> Subst a -> Subst a
restrict ss xs = [ (v, e) | (v, e) <- xs, v `elem` ss ]

combine theta zeta  =
    let ss    = map fst theta
        zeta' = [ (v, e) | (v, e) <- zeta, v `notElem` ss ]
    in  subst theta zeta ++ zeta'


dom theta   = map fst theta
range theta = concatMap fv (map snd theta)
vars theta  = dom theta `union` range theta


isRenamingSubst s = all (\(_, x) -> isVar x) s
