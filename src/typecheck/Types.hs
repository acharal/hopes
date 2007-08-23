module Types where

import Pretty
import List (nub)

type TyVar = Int

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
order (TyFun t t') = 1 + max (order t) (order t')
order _ = 0

tyargs :: Type -> [Type]
tyargs (TyFun t t') = h t ++ tyargs t'
    where   h (TyTup tl) = tl
            h t = [t]
tyargs _ = []

-- pretty printing of types

tyvarnames :: [String]
tyvarnames =
    let letters = [ "a", "b", "c", "e", "f" ]
    in  letters ++ [ x++(show i) | x <- letters, i <- [1..] ]

instance Pretty MonoType where
    ppr (TyCon TyBool)   = text "o"
    ppr (TyCon TyAll)    = text "i"
    ppr ty@(TyFun t t')  = 
        let f = vna ty
        in  sep [ pprWithV f t , arrow <+> pprWithV f t' ]
    ppr ty@(TyTup tl)    =
        parens $ sep (punctuate comma (map (pprWithV (vna ty)) tl))
    ppr ty@(TyVar t)     = pprWithV (vna ty) ty

pprWithV f (TyTup tl)    = parens $ sep (punctuate comma (map (pprWithV f) tl))
pprWithV f (TyFun t t')  = sep [ pprWithV f t , arrow <+> pprWithV f t' ]
pprWithV f (TyVar v)     = text $ f v
pprWithV _ t             = ppr t

-- var - name map
vna ty x = 
    let tyv = tyvars ty
        fl  = zip tyv tyvarnames
    in case lookup x fl of
            Nothing -> show x
            Just n -> n
