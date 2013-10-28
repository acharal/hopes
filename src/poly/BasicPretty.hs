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

module BasicPretty (
        module Pretty
    ) where

import Pretty

import Pos 
import Types 


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
