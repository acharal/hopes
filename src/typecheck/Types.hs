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

tyBool = TyCon TyBool
tyAll  = TyCon TyAll

instance Pretty MonoType where
    ppr (TyCon TyBool) = text "o"
    ppr (TyCon TyAll)  = text "i"
    ppr (TyFun t t')   = sep [ ppr t , arrow <+> ppr t' ]
    ppr (TyTup tl)     = parens $ sep (punctuate comma (map ppr tl))
    ppr (TyVar t)      = int t