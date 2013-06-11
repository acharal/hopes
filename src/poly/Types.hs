--  Copyright (C) 2013 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--                     Emmanouil Koukoutos   <manoskouk@softlab.ntua.gr>
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


module Types where

import Prelude hiding (concatMap, foldl, foldr)
import Basic
import Data.List (nub)
import Data.Foldable hiding (maximum,concat)
import Data.Monoid
import Pos(HasPosition(..))

-- Argument variables
newtype Alpha = Alpha Symbol
  deriving Eq
-- Predicate variables
newtype Phi = Phi Symbol
  deriving Eq

-- Non generalized predicate
data PiType = Pi_o 
            | Pi_fun [RhoType] PiType
            | Pi_var Phi
    deriving Eq
-- Argument
data RhoType = Rho_i
             | Rho_pi PiType
             | Rho_var Alpha
    deriving Eq
-- Functional with a no. of arguments
data FunType = Fun Int
-- Polymorphic
data PolyType = Poly_gen [Alpha] [Phi] PiType
--    deriving Eq -- FIXME: modulo alpha-conversion
{-
-- All types. FIXME: needed???
data AllTypes = All_fun  FunType 
              | All_rho  RhoType 
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
-}


-- Pretty printing for types

instance Show Alpha where
  showsPrec p (Alpha alpha) = ( alpha ++ )

instance Show Phi where
  showsPrec p (Phi phi) = ( phi ++ )

instance Show FunType where
    showsPrec p (Fun n) = ("(" ++). walk n . (") -> i" ++)
        where walk 1 = ("i" ++)
              walk n = ("i, " ++) . walk (n-1)

instance Show RhoType where
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
      walk alphas . walk phis . showsPrec p pi
      where walk [] = id
            walk (x : xs) = ("∀" ++) . showsPrec p x . (". " ++) . walk xs

-- Type transformations, from mono- to poly
piToPoly = Poly_gen [] []

polyToPi (Poly_gen [] [] pi) = pi
polyToPi poly = error $ "Monomorphism violation: " ++ show poly


-- | Wrapper type for rho-typed objects

data Typed a = T a RhoType deriving Show

instance Eq a => Eq (Typed a) where
    (T a tya) == (T b tyb) = a == b

typed ty a = T a ty
unTyp (T a _) = a

class HasType a where
    typeOf :: a -> RhoType
    hasType :: RhoType -> a -> a
    
    
instance HasType RhoType where
    typeOf = id
    hasType t _ = t

instance HasType (Typed a) where
    typeOf (T _ ty) = ty
    hasType ty (T a _) = T a ty

--instance Functor Typed where
--    fmap f (T a ty) = T (f a) ty


-- Combine type with location
instance HasPosition a => HasPosition (Typed a) where 
    posSpan (T a _) = posSpan a


order :: HasType a => a -> Int
order a =
    case typeOf a of
        (Rho_pi (Pi_fun rhos pi)) -> 
            max (1 + maximum(map order rhos)) (order $ Rho_pi pi)
        
        (Rho_pi Pi_o) -> 0
        (Rho_i) -> 0
        _  -> error ("no fixed order when type is variable")

instance HasArity RhoType where 
    arity (Rho_pi (Pi_fun rhos pi)) = Just $ length rhos
    arity (Rho_pi Pi_o)             = Just 0
    arity (Rho_i)                   = Just 0
    arity r = Nothing


