--  Copyright (C) 2007 2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Bindings where

import Types

data Binding a = Bind (Typed a)

data Bindings a = [ Binding a ]

instance HasType (Binding a) where
    typeOf (Bind b) = typeOf b

symbolBind (Bind a) = unTyp a

class Eq b => HasBindings a b where
    binds  :: a -> Bindings b
    isBind :: a -> b -> Bool
    isBind a s = any (s==) $ map symbolBind (binds a)

lookupBind :: Eq a => a -> Binds a -> Maybe (Bind a)
lookupBind x = find ((x==).symbolBind)