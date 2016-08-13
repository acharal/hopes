--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Pretty (
        module Pretty,
        module Text.PrettyPrint
    ) where

import Text.PrettyPrint

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


dcolon  = text "::"
arrow   = text "->"
dot     = char '.'
entails = text ":-"
slash   = char '/'
-- semi = text ";"
curly a = text "{" <+> a <+> text "}"
--brackets a = text "[" <+> a <+> text "]"
