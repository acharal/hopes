module Types where

import List (nub)
import Data.IORef
import Prelude hiding (concatMap, foldl, foldr)
import Data.Foldable hiding (maximum)
import Data.Monoid

data TyVar = Tv Int (IORef (Maybe Type))

instance Eq TyVar where
    (Tv i _) == (Tv j _) = i==j

instance Ord TyVar where
    compare (Tv i _) (Tv j _) = compare i j

instance Show TyVar where
    showsPrec p (Tv i _) = showsPrec p i

-- if language supports polymorphism then Type can be MonoType or PolyType (with quantified tyvars)
type Type     = TypeV TyVar
type MonoType = MonoTypeV TyVar

type TypeV    = MonoTypeV

-- monotype parametrized by the tyvar
data MonoTypeV a =
      TyVar a
    | TyGrd GrdType
    | TyFun (MonoTypeV a) (MonoTypeV a)
    | TyTup [MonoTypeV a]
  deriving (Eq, Show)

data GrdType = TyAll | TyBool deriving (Eq,Show)

liftGround = TyGrd

instance Foldable MonoTypeV where
    foldMap f (TyVar a)     = f a
    foldMap f (TyGrd c)     = mempty
    foldMap f (TyFun t1 t2) = foldMap f t1 `mappend` foldMap f t2
    foldMap f (TyTup tl)    = mconcat (map (foldMap f) tl)

instance Functor MonoTypeV where
    fmap f (TyVar a)     = TyVar (f a)
    fmap f (TyGrd c)     = TyGrd c
    fmap f (TyFun t1 t2) = TyFun (fmap f t1) (fmap f t2)
    fmap f (TyTup tl)    = TyTup (map (fmap f) tl)


tyBool    = liftGround TyBool
tyAll     = liftGround TyAll
bogusType = error ("This type is a placeholder and must not be evaluated")

tyargs :: TypeV a -> [TypeV a]
tyargs (TyFun t t') = h t ++ tyargs t'
    where   h (TyTup tl) = tl
            h t = [t]
tyargs _ = []

tyvars ty = nub $ foldMap (:[]) ty

data Typed a = T a Type

instance Eq a => Eq (Typed a) where
    (T a tya) == (T b tyb) = a == b

typed ty a = T a ty
unTyp (T a _) = a

class HasType a where
    typeOf :: a -> Type

instance HasType Type where
    typeOf = id

instance HasType (Typed a) where
    typeOf (T _ ty) = ty

instance HasType (TySig a) where
    typeOf (_, t) = t

instance Functor Typed where
    fmap f (T a ty) = T (f a) ty


order :: HasType a => a -> Int
order a = 
    case typeOf a of
        (TyFun t t') -> max (1 + (order t)) (order t')
        (TyTup tys)  -> maximum (map order tys)
        (TyVar _)    -> error ("no fixed order when type is variable")
        _            -> 0

arity :: HasType a => a -> Int
arity a = 
    let count (TyTup tys) = length tys
        count _ = 1
    in  case typeOf a of
            TyFun t t' -> count t + arity t'
            _ -> 0

type TySig a = (a, Type)

type TyEnv a = [ TySig a ]

emptyTyEnv :: TyEnv a
emptyTyEnv = []

lookupTyEnv :: Eq a => a -> TyEnv a -> Maybe Type
lookupTyEnv = lookup

findTySig :: Eq a => a -> TyEnv a -> Maybe (TySig a)
findTySig a = find (\(b, _) -> b == a)

