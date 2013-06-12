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
 - Basic definitions for polyHOPES compiler
 -}

module Basic where

-- Symbol is an alias for the string type 
type Symbol = String

-- A class describing a data structure that can be 
-- "flattened" into a list
class Flatable a where 
    flatten :: a -> [a]


-- Class for types with arity, inplemented as a partial
-- function
class HasArity a where 
    arity :: a -> Maybe Int

-- Types with a descriptive name
class HasName a where
    nameOf :: a -> Symbol
    

-- The reverse application/forward pipe operator. Thanks F#!
infixl 0 |>
x |> f = f x

