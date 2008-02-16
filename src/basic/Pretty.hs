{-# OPTIONS -fglasgow-exts #-}
module Pretty (
        module Pretty,
        module Text.PrettyPrint
    ) where

import Text.PrettyPrint

import Loc
import Symbol
import Types
import List (nub)

class Pretty a where
    ppr :: a -> Doc

pprint a = print (ppr a)

instance Pretty Doc where
    ppr = id

instance Pretty [Char] where
    ppr = text

instance Pretty Int where
    ppr = int

dcolon  = text "::"
arrow   = text "->"
dot     = char '.'
entails = text ":-"
curly a = text "{" <+> a <+> text "}"

instance Pretty Loc where
    ppr (Loc _ (-1) (-1)) = text "<no-location>"
    ppr (Loc f l c) = hcat $ punctuate colon [ text f, int l, int c ]

instance Pretty LocSpan where
    ppr (OneLineSpan f l c1 c2) =
        hcat $ punctuate colon [ text f, int l, parens $ int c1 <> char '-' <> int c2 ]
    ppr (MultiLineSpan f l1 c1 l2 c2) = 
        hcat $ punctuate colon [ text f, ppr_par l1 c1 <> char '-' <> ppr_par l2 c2 ]
        where ppr_par l c = parens (int l <> comma <> int c)
    ppr (LocSpan l1 l2) = ppr l1 <> char '-' <> ppr l2

instance Pretty a => Pretty (Symbol a) where
    ppr (Sym s) = ppr s
    ppr AnonSym = text "_"

instance Pretty a => Pretty (Typed a) where
    ppr (T a ty) = ppr a <+> curly (ppr ty)

instance Pretty GrdType where
    ppr TyBool = text "o"
    ppr TyAll  = text "i"

instance Pretty TyVar where
    ppr (Tv i _) = int i

instance (Eq a, Pretty a) => Pretty (MonoTypeV a) where
    ppr t = pprPrec 1 f t
        where f = tvmap [t]

pprPrec p f (TyTup tl)     = parens $ sep (punctuate comma (map (pprPrec 1 f) tl))
pprPrec p f (TyGrd c)      = ppr c
pprPrec p f (TyVar v)      = f v
pprPrec p f ty@(TyFun t t')= 
    if (p == 0) then 
        parens (sep [ pprPrec 0 f t , arrow <+> pprPrec p f t' ])
    else
        sep [ pprPrec 0 f t , arrow <+> pprPrec p f t' ]

tynames = letters ++ [ x++(show i) | x <- letters, i <- [1..] ]
    where letters = [ "a", "b", "c", "d", "e", "f" ]

tvmap tys v = 
    let tvs = nub $ concatMap tyvars tys
        fl  = zip tvs tynames
    in case lookup v fl of
            Nothing -> ppr v
            Just n  -> text n


instance Pretty a => Pretty (TySig a) where
    ppr (a,t) = sep [ ppr a, dcolon <+> ppr t]

instance Pretty a => Pretty (TyEnv a) where
    ppr ts = vcat $ map ppr ts
