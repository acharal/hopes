module Types where

import Pretty
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
    | TyCon TyCon
    | TyFun (MonoTypeV a) (MonoTypeV a)
    | TyTup [MonoTypeV a]
  deriving (Eq, Show)

instance Foldable MonoTypeV where
    foldMap f (TyVar a)     = f a
    foldMap f (TyCon c)     = mempty
    foldMap f (TyFun t1 t2) = foldMap f t1 `mappend` foldMap f t2
    foldMap f (TyTup tl)    = mconcat (map (foldMap f) tl)

instance Functor MonoTypeV where
    fmap f (TyVar a)     = TyVar (f a)
    fmap f (TyCon c)     = TyCon c
    fmap f (TyFun t1 t2) = TyFun (fmap f t1) (fmap f t2)
    fmap f (TyTup tl)    = TyTup (map (fmap f) tl)

type TySign a = (a, Type)

data TyCon =
      TyAll
    | TyBool
  deriving (Eq,Show)

tyBool = TyCon TyBool
tyAll  = TyCon TyAll

tyvars ty = nub $ foldMap (\x -> [x]) ty

order :: Type -> Int
order (TyFun t t') = max (1 + (order t)) (order t')
order (TyTup tys)  = maximum (map order tys)
order (TyVar _)    = error ("no fixed order when type is variable")
order _            = 0

arity :: Type -> Int
arity (TyFun t t') = count t + arity t'
    where count (TyTup tys) = length tys
          count _ = 1
arity _ = 0

tyargs :: Type -> [Type]
tyargs (TyFun t t') = h t ++ tyargs t'
    where   h (TyTup tl) = tl
            h t = [t]
tyargs _ = []

-- pretty printing of types

tynames = letters ++ [ x++(show i) | x <- letters, i <- [1..] ]
    where letters = [ "a", "b", "c", "d", "e", "f" ]

instance Pretty TyCon where
    ppr TyBool = text "o"
    ppr TyAll  = text "i"

instance Pretty MonoType where
    ppr t = pprPrec 1 f t
        where f = tvmap [t]

pprPrec p f (TyTup tl)     = parens $ sep (punctuate comma (map (pprPrec 1 f) tl))
pprPrec p f (TyCon c)      = ppr c
pprPrec p f (TyVar v)      = text (f v)
pprPrec p f ty@(TyFun t t')= 
    if (p == 0) then 
        parens (sep [ pprPrec 1 f t , arrow <+> pprPrec p f t' ])
    else
        sep [ pprPrec 0 f t , arrow <+> pprPrec p f t' ]


tvmap tys v = 
    let tvs = nub $ concatMap tyvars tys
        fl  = zip tvs tynames
    in case lookup v fl of
            Nothing -> show v
            Just n -> n
