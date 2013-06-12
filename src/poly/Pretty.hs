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

{- 
 - Pretty printing
 -}

module Pretty (
        module Pretty,
        module Text.PrettyPrint
    ) where

import Text.PrettyPrint

import Pos 
import Types 


class Pretty a where
    ppr :: a -> Doc

pprint a = print (ppr a)

instance Pretty Doc where
    ppr = id

instance Pretty [Char] where
    ppr = text

instance Pretty Int where
    ppr = int

instance Pretty Integer where
    ppr = text . show

instance Pretty Double where
    ppr = text . show

instance Pretty a => Pretty (Maybe a) where
    ppr (Just a) = ppr a
    ppr Nothing  = empty

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    ppr (Left  a) = ppr a
    ppr (Right b) = ppr b

dcolon  = text "::"
arrow   = text "->"
dot     = char '.'
entails = text ":-"
slash   = char '/'
-- semi = text ";"
curly a = text "{" <+> a <+> text "}"
--brackets a = text "[" <+> a <+> text "]"

instance Pretty SourcePos where
    ppr sp = 
        if sp == bogusPos
            then text "<no-location>"
            else hcat $ punctuate colon [ text (sourceName sp), int (sourceLine sp), int (sourceColumn sp) ]

instance Pretty PosSpan where
    ppr (OneLineSpan f l c1 c2) =
        hcat $ punctuate colon [ text f, int l, parens $ int c1 <> char '-' <> int c2 ]
    ppr (MultiLineSpan f l1 c1 l2 c2) = 
        hcat $ punctuate colon [ text f, ppr_par l1 c1 <> char '-' <> ppr_par l2 c2 ]
        where ppr_par l c = parens (int l <> comma <> int c)
    ppr (PosSpan l1 l2) = ppr l1 <> char '-' <> ppr l2

instance Show PosSpan where
    show = show . ppr

-- Types
instance Pretty RhoType where
    ppr rho = text $ show rho

instance Pretty PiType where
    ppr pi = text $ show pi

instance Pretty PolyType where
    ppr poly = text $ show poly

instance Pretty FunType where
    ppr f = text $ show f


{-
tynames = letters ++ [ x++(show i) | x <- letters, i <- [1..] ]
    where letters = [ "a", "b", "c", "d", "e", "f" ]

tvmap tys v = 
    let tvs = nub $ concatMap tyvars tys
        fl  = zip tvs tynames
    in case lookup v fl of
            Nothing -> ppr v
            Just n  -> text n
-}
{-
instance Pretty a => Pretty (RhoSig a) where
    ppr (a,t) = sep [ ppr a, dcolon <+> ppr t]

instance Pretty a => Pretty (PolySig a) where
    ppr (a,t) = sep [ ppr a, dcolon <+> ppr t]

instance (Pretty a, Pretty b) => Pretty (TyEnv a b) where
    ppr ts = vcat $ map ppr (fst ts) ++ map ppr (snd ts)
-}
