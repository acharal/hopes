--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PATICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Types where

import Prelude hiding (concatMap, foldl, foldr)
import Basic
import Data.List (nub)
import Data.Foldable hiding (maximum)
import Data.Monoid
import Loc(HasLocation, Located(..))

-- Argument variables
newtype Alpha = Alpha String
  deriving Eq
-- Predicate variables
newtype Phi = Phi String
  deriving Eq

-- Non generalized predicate
data PiType = Pi_o 
            | Pi_fun [Type] PiType
            | Pi_var Phi
    deriving Eq
-- Argument
data Type = Rho_i
          | Rho_pi PiType
          | Rho_var Alpha
    deriving Eq
-- Functional with a no. of arguments
data FunType = Fun Int
-- Polymorphic
data PolyType = Poly_gen [Alpha] [Phi] PiType
    deriving Eq -- FIXME: modulo alpha-conversion

-- All types. FIXME: needed???
data AllTypes = All_fun  FunType 
              | All_rho  Type 
              | All_poly PolyType
   
instance Eq AllTypes where 
    All_fun (Fun n) == All_fun (Fun n') = n == n'
    All_rho rho == All_rho rho' = 
        rho == rho'
    All_poly poly == All_poly poly' = 
        poly == poly' 
    All_rho (Rho_pi pi) == All_poly (Poly_gen [] [] pi') =
        pi == pi' -- FIXME : modulo alpha conversion ??? Prob not!
    All_poly (Poly_gen [] [] pi') == All_rho (Rho_pi pi) =
        pi == pi' -- FIXME : modulo alpha conversion ??? Prob not!
    _ == _ = False


{-
class (Show tp, Eq tp) => Type tp 
instance Type PiType
instance Type Type 
instance Type PolyType
-}

-- Pretty printing for types

instance Show Alpha where
  showsPrec p (Alpha alpha) = (alpha ++)

instance Show Phi where
  showsPrec p (Phi phi) = (phi ++)

instance Show FunType where
  showsPrec p (Fun n) = ("(" ++). walk n . (") -> i" ++)
    where walk 1 = ("i" ++)
          walk n = ("i, " ++) . walk (n-1)

instance Show Type where
  showsPrec p Rho_i = ("i" ++)
  showsPrec p (Rho_pi pi) = showsPrec p pi
  showsPrec p (Rho_var alpha) = showsPrec p alpha

instance Show PiType where
  showsPrec p Pi_o = ("o" ++)
  showsPrec p (Pi_fun rhos pi) =
    ("(" ++) . walk rhos . (") -> " ++) . showsPrec p pi
    where walk [rho] = showsPrec p rho
          walk (rho : rhos) = showsPrec p rho . (", " ++) . walk rhos
  showsPrec p (Pi_var phi) = showsPrec p phi

instance Show PolyType where
  showsPrec p (Poly_gen alphas phis pi) =
    walk alphas . walk' phis . showsPrec p pi
    where walk [] = id
          walk (x : xs) = ("∀" ++) . showsPrec p x . (". " ++) . walk xs
          walk' [] = id
          walk' (x : xs) = ("∀" ++) . showsPrec p x . (". " ++) . walk' xs

-- FIXME: Question: why does polymorphic walk not work???

--instance Foldable MonoTypeV where
--    foldMap f (TyVar a)     = f a
--    foldMap f (TyGrd c)     = mempty
--    foldMap f (TyFun t1 t2) = foldMap f t1 `mappend` foldMap f t2
--    foldMap f (TyTup tl)    = mconcat (map (foldMap f) tl)



--instance Functor MonoTypeV where
--    fmap f (TyVar a)     = TyVar (f a)
--    fmap f (TyGrd c)     = TyGrd c
--    fmap f (TyFun t1 t2) = TyFun (fmap f t1) (fmap f t2)
--    fmap f (TyTup tl)    = TyTup (map (fmap f) tl)


--tyargs :: TypeV a -> [TypeV a]
--tyargs (TyFun t t') = t : tyargs t'
--    --where   h (TyTup tl) = tl
--    --        h t = [t]
--tyargs _ = []

--tyvars ty = nub $ foldMap (:[]) ty


-- | Wrapper type for rho-typed objects

data Typed a = T a Type deriving Show

instance Eq a => Eq (Typed a) where
    (T a tya) == (T b tyb) = a == b

typed ty a = T a ty
unTyp (T a _) = a

class HasType a where
    typeOf :: a -> Type
    hasType :: Type -> a -> a
    
    
instance HasType Type where
    typeOf = id
    hasType t _ = t

instance HasType (Typed a) where
    typeOf (T _ ty) = ty
    hasType ty (T a _) = T a ty

instance Functor Typed where
    fmap f (T a ty) = T (f a) ty


instance HasType (TySig a) where
    typeOf (_, t) = t
    hasType ty (a, _) = (a, ty)

-- Combine type with location
instance HasType a => HasType (Located a) where
    typeOf (L _ a) = typeOf a
    hasType ty (L l a) = L l (hasType ty a)

order :: HasType a => a -> Int
order a =
    case typeOf a of
        (Rho_pi (Pi_fun rhos pi)) -> 
            max (1 + maximum(map order rhos)) (order $ Rho_pi pi)
        
        (Rho_pi Pi_o) -> 0
        (Rho_i) -> 0
        _  -> error ("no fixed order when type is variable")

instance HasArity Type where 
    arity (Rho_pi (Pi_fun rhos pi)) = Just $ length rhos
    arity (Rho_pi Pi_o)             = Just 0
    arity (Rho_i)                   = Just 0
    arity r = Nothing

-- | Monomorphic type signature (variables)
type TySig a = (a, Type)
-- | Polymorphic type signature (predicates)
type PolySig a = (a, PolyType)

-- | type environment is a set of type signatures for
-- variables and polymorphic predicates
type TyEnv a b = ( [ TySig a ], [ PolySig b])

-- emptyTyEnv :: TyEnv a
-- emptyTyEnv = []

lookupType :: Eq a => a -> TyEnv a b-> Maybe Type
lookupType a = (lookup a).fst

lookupPoly :: Eq b => b -> TyEnv a b-> Maybe PolyType
lookupPoly a = (lookup a).snd


--findTySig :: Eq a => a -> TyEnv a b -> Maybe (TySig a)
--findTySig a = find (\(b, _) -> b == a)


