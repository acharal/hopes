--  Copyright (C) 2006-2012 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Language.Hopl.Syntax.Pretty () where

import Pretty
import Loc (unLoc)
import Language.Hopl.Syntax


-- syntax

instance Pretty a => Pretty (HpExpr a) where
    ppr (HpAnn e ty)  = hsep [ ppr (unLoc e), dcolon, ppr ty ]
    ppr (HpPar e)     = parens (ppr (unLoc e))
    ppr (HpSym s)     = ppr s
    ppr (HpApp e es)  = ppr (unLoc e) <>
                            parens (sep (punctuate comma (map (ppr.unLoc) es)))
    ppr (HpTup es)    = parens (sep (punctuate comma (map (ppr.unLoc) es)))
    ppr (HpLam xs e)  =
        sep (punctuate (text "->") (map (\x -> text "\\" <> ppr (symbolBind x)) xs)) <>
            text "->" <+> ppr (unLoc e)

instance Pretty a => Pretty (HpClause a) where
    ppr (HpClause _ [h] []) = ppr (unLoc h) <> dot
    ppr (HpClause _ h b)  =
        hang (  sep (punctuate comma (map (ppr.unLoc)  h)) <> entails) 4 $ 
                sep (punctuate comma (map (ppr.unLoc)  b)) <> dot


instance Pretty a => Pretty (HpSrc a) where
    ppr p = vcat $ map (ppr.unLoc) (clauses p)

