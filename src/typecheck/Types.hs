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
