--  Copyright (C) 2006-2012 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

{-# LANGUAGE
    FlexibleInstances
   ,MultiParamTypeClasses
   ,NoMonomorphismRestriction
#-}

-- | Higher Order Predicate Language
module Language.Hopl where

import Lang
import Types
import Data.Monoid

data Expr a =
       App (Expr a) (Expr a)
    |  Flex a
    |  Rigid a
    |  Lambda a (Expr a)
  deriving (Eq, Show)

data Clause a = C a (Expr a) deriving Eq

isFlex (Flex _) = True
isFlex _ = False

isRigid (Rigid _) = True
isRigid _ = False

clauseHead (C a _) = a
clauseBody (C _ b) = b

type Goal a = Expr a

cons = Rigid $ liftSym "."
nil  = Rigid $ liftSym "[]"

instance (Symbol a, Eq a)  => HasLogicConstants (Expr a) where
    ctop    = Rigid $ liftSym "true"
    cbot    = Rigid $ liftSym "false"
    ceq     = Rigid $ liftSym "="
    cexists = Rigid $ liftSym "_exists"
    cor     = Rigid $ liftSym ";"
    cand    = Rigid $ liftSym ","

isCut sym = sym == (Rigid $ liftSym "!")
isNot sym = sym == (Rigid $ liftSym "not")

{-
instance (HasConstants a, HasType a) => HasConstants (Typed a) where
    ctop = typed tyBool ctop
    cbot = typed tyBool cbot
    ceq = typed (TyFun tyAll tyAll) ceq
    cexists = typed (TyFun (TyVar undefined) tyBool) cexists
    cor = typed (TyFun tyBool ((TyFun tyBool) tyBool)) cor
    cand = typed (TyFun tyBool ((TyFun tyBool) tyBool)) cand
-}
{-
instance (Eq a, Symbol a) => HasConstants (Expr (Typed a)) where
    ctop = Const (typed tyBool (liftSym "1"))
    cbot = Const (typed tyBool (liftSym "0"))
    ceq  = Const (typed (TyFun tyAll tyAll) (liftSym "="))
    cexists = Const (typed (TyFun (TyVar undefined) tyBool) (liftSym "exists"))
    cor = Const (typed (TyFun tyBool ((TyFun tyBool) tyBool)) (liftSym "or"))
    cand = Const (typed (TyFun tyBool ((TyFun tyBool) tyBool)) (liftSym "and"))
    isEqual(Const (T s _)) = s == liftSym "="
    isConj (Const (T s _)) = s == liftSym "and"
    isDisj (Const (T  s _)) = s == liftSym "or"
    isTop (Const (T s _)) = s == liftSym "1"
    isBot (Const (T s _)) = s == liftSym "0"
    isExistsQ (Const (T s _)) = s == liftSym "exists"
-}

{-
contradiction :: HasLogicConstants a => a
contradiction = ctop

isContra :: HasLogicConstants a => a -> Bool
isContra = (==contradiction)
-}

instance Symbol a => HasSignature (Expr a) a where
        sig (App e1 e2) = sig e1 `mappend` sig e2
        sig (Flex a)  = varSig a
        sig (Rigid a) = rigSig a
        sig (Lambda a x) = varSig a `mappend` sig x

instance Symbol a => HasSignature (Clause a) a where
        sig (C p e2) = rigSig p `mappend` sig e2 `mappend` mempty

--instance HasSignature (Goal a) a where
--        sig gs = mconcat $ map sig gs

instance (Symbol a, Eq a, HasType a) => HasType (Expr a) where
        typeOf (App e1 e2) =
            case typeOf e1 of
                (TyFun a b) -> b
                t -> error ("not expected type " ++ (show t))
        typeOf (Flex a)  = typeOf a
        typeOf c@(Rigid a)
            | c == ctop = tyBool
            | c == cbot = tyBool
            | c == cor  = TyFun tyBool ((TyFun tyBool) tyBool)
            | c == cand = TyFun tyBool ((TyFun tyBool) tyBool)
            | c == ceq  = TyFun tyAll ((TyFun tyAll) tyBool)
            | c == cexists = TyFun (TyVar undefined) tyBool
            | otherwise = typeOf a
        typeOf (Lambda a e) =
            let t = typeOf e
                t1 = typeOf a
            in TyFun t1 t
        hasType ty e = error "hasType"

instance HasType a => HasType (Clause a) where
        typeOf c = liftGround TyBool
        hasType ty c =
            if not (ty == tyBool) then
                error "clause can have not boolean type"
            else c

-- instance HasType a => HasType (Goal a) where
--        typeOf g = liftGround TyBool

-- liftSet :: Expr a -> Expr a
-- liftSet e@(Set _ _) = e
-- liftSet (Flex v) = Set [] [v]
-- liftSet _ = error ("Cannot represent expression as set")

functor :: Expr a -> Expr a
functor (App e a) = functor e
functor e = e

args (App e a) = args2 e ++ [a]
    where args2 (App e a) = args2 e ++ [a]
          args2 e = []


data KnowledgeBase a =
    KB {
        -- name :: String
        clauses :: [Clause a]
    }

instance Symbol a => HasSignature (KnowledgeBase a) a where
        sig cs = mconcat $ map sig $ clauses cs

instance Monoid (KnowledgeBase a) where
    mempty  = KB mempty
    mappend (KB x) (KB y) = KB $ mappend x y
