module Types where

import Pretty

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

tvs :: MonoType -> [TyVar]
tvs (TyVar v) = [v]
tvs (TyFun ty1 ty2) = tvs ty1 ++ tvs ty2
tvs (TyTup tys) = concatMap tvs tys
tvs _ = []


instance Pretty MonoType where
    ppr (TyCon TyBool) = text "o"
    ppr (TyCon TyAll)  = text "i"
    ppr (TyFun t t')   = sep [ ppr t , arrow <+> ppr t' ]
    ppr (TyTup tl)     = parens $ sep (punctuate comma (map ppr tl))
    ppr (TyVar t)      = int t