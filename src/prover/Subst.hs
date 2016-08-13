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
import Core (VarSym, CExpr(..), Flex(..), isVar, fv, namedVars, isNamedCVar)
import Types
import Loc
import Data.List (union)

type Subst = [ (Flex, CExpr) ]

success :: Subst
success = []

bind :: Flex -> CExpr -> Subst
bind v e = [(v, e)]


-- isTaut :: Eq a => (a, CExpr a) -> Bool
isTaut (v, CVar v') = v == v'
isTaut _ = False

class Substitutable a where
	subst :: Substitutable a => Subst -> a -> a

instance Substitutable CExpr where
        subst theta (CApp i e1 es)   = CApp i (subst theta e1) (map (subst theta) es)
        subst theta (CAnd i e1 e2)   = CAnd i (subst theta e1) (subst theta e2)
        subst theta (COr  i e1 e2)   = COr  i (subst theta e1) (subst theta e2)
        subst theta (CEq  e1 e2)     = CEq  (subst theta e1) (subst theta e2)
        subst theta (CLambda i xs e)  = CLambda i xs (subst theta' e)
						where theta' = theta `except` xs
        subst theta (CExists i x e) = CExists i x (subst theta' e)
						where theta' = theta `except` [x]
        subst theta e@(CVar v) = maybe e id $ lookup v theta
        subst theta (CNot e) = CNot (subst theta e)
        subst theta (CCons e1 e2) = CCons (subst theta e1) (subst theta e2)
        subst theta e = e


instance Substitutable Subst where
	subst theta zeta = [ (v, e') | (v, e) <- theta, let e' = subst zeta e, not (isTaut (v, e')) ]


restrict :: [Flex] -> Subst -> Subst
restrict ss xs = [ (v, e) | (v, e) <- xs, v `elem` ss ]

except :: Subst -> [Flex] -> Subst
except s xs = [ (v, e) | (v, e) <- s, v `notElem` xs ]

combine theta zeta  =
    let ss    = map fst theta
        zeta' = [ (v, e) | (v, e) <- zeta, v `notElem` ss ]
    in  subst theta zeta ++ zeta'


dom theta   = map fst theta
range theta = concatMap fv (map snd theta)
vars theta  = dom theta `union` range theta


isRenamingSubst s = all (\(_, x) -> isVar x) s
