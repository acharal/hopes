module Types where

import Pretty
import List (nub)
import Data.IORef

data TyVar = Tv Int (IORef (Maybe Type))

instance Eq TyVar where
    (Tv i _) == (Tv j _) = i==j

instance Ord TyVar where
    compare (Tv i _) (Tv j _) = compare i j

instance Show TyVar where
    showsPrec p (Tv i _) = showsPrec p i

-- if language supports polymorphism then Type can be MonoType or PolyType (with quantified tyvars)
type Type  = MonoType

data TyCon =
      TyAll
    | TyBool
  deriving (Eq,Show)

data MonoType =
      TyVar TyVar
    | TyCon TyCon
    | TyFun MonoType MonoType
    | TyTup [MonoType]
  deriving (Eq, Show)

tyBool = TyCon TyBool
tyAll  = TyCon TyAll

tyvars' :: Type -> [TyVar]
tyvars' (TyVar v)    = [v]
tyvars' (TyTup tys)  = concatMap tyvars' tys
tyvars' (TyFun t t') = tyvars' t ++ tyvars' t'
tyvars' _ = []
tyvars t = nub $ tyvars' t

order :: Type -> Int
order (TyFun t t') = max (1 + (order t)) (order t')
order (TyTup tys)  = maximum (map order tys)
order _ = 0

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

tyvarnames :: [String]
tyvarnames =
    let letters = [ "a", "b", "c", "d", "e", "f" ]
    in  letters ++ [ x++(show i) | x <- letters, i <- [1..] ]


instance Pretty MonoType where
    ppr t = pprPrec 1 f t
        where f = vna t

pprPrec p f (TyTup tl)     = parens $ sep (punctuate comma (map (pprPrec 1 f) tl))
pprPrec p f (TyCon TyBool) = text "o"
pprPrec p f (TyCon TyAll)  = text "i"
pprPrec p f (TyVar v)      = text (f v)
pprPrec p f ty@(TyFun t t')= 
    if (p == 0) then 
        parens (sep [ pprPrec 1 f t , arrow <+> pprPrec p f t' ])
    else
        sep [ pprPrec 0 f t , arrow <+> pprPrec p f t' ]


-- var - name map
vna ty x = 
    let tyv = tyvars ty
        fl  = zip tyv tyvarnames
    in case lookup x fl of
            Nothing -> show x
            Just n -> n
