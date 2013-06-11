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



-- Model a location in a program
module Pos (
    module Pos, 
    module Text.Parsec.Pos,
    module Data.Monoid
) where

import Text.Parsec.Pos
import Data.Monoid hiding ((<>))
import Control.Monad.Identity

-- A position range
data PosSpan = 
      PosSpan SourcePos SourcePos
    | OneLineSpan   FilePath Line Column Column
    | MultiLineSpan FilePath Line Column Line Column
  deriving (Eq)



bogusPos  = newPos "bogusFile" (-1) (-1)
bogusSpan = PosSpan bogusPos bogusPos

-- Start of a span/range
spanBegin (OneLineSpan f l c1 c2)       = newPos f l c1
spanBegin (MultiLineSpan f l1 c1 l2 c2) = newPos f l1 c1
spanBegin (PosSpan l1 l2)               = l1

-- End of a span
spanEnd (OneLineSpan f l c1 c2)         = newPos f l c2
spanEnd (MultiLineSpan f l1 c1 l2 c2)   = newPos f l2 c2
spanEnd (PosSpan l1 l2)                 = l2


mkSpan :: SourcePos -> SourcePos -> PosSpan
mkSpan l1 l2 =
    if sourceName l1  == sourceName l2 then
        if sourceLine l1 == sourceLine l2 then
            OneLineSpan (sourceName l1) (sourceLine l1) (sourceColumn l1) (sourceColumn l2)
        else
            MultiLineSpan (sourceName l1) (sourceLine l1) (sourceColumn l1) 
                                          (sourceLine l2) (sourceColumn l2)
    else
        PosSpan l1 l2


-- Location is a Monoid with neutral element = bogusPos
instance Monoid SourcePos where
    mempty = bogusPos
    mappend a b
        | a == bogusPos = b
        | otherwise = a

instance Monoid PosSpan where
    mempty = bogusSpan
    mappend a b
        | a == bogusSpan = b
        | otherwise = mkSpan (spanBegin a) (spanEnd b)


-- | Class which is implemented by syntactic structures, 
-- errors and of course positions themselves. Ensures a
-- position and a position span.
-- Minimum definition: pos OR posSpan
class HasPosition a where
    pos     :: a -> SourcePos
    posSpan :: a -> PosSpan
    posSpan x = mkSpan s s where s = pos x
    pos x     = spanBegin (posSpan x)

instance HasPosition SourcePos where
    pos x = x
instance HasPosition PosSpan where
    posSpan x = x





{-
instance (HasLocation a, HasLocation b) => HasLocation (a, b) where
    locSpan (a, b) = locSpan a `mappend` locSpan b

instance HasLocation a => HasLocation [a] where
    loc xs = mconcat (map loc xs)
    locSpan xs = mconcat (map locSpan xs)


-- | Add location to a value
data Located a = L LocSpan a deriving Eq

instance HasLocation (Located a) where
    locSpan (L l _) = l

instance Functor Located where
    fmap f (L l x) = L l (f x)


unLoc :: Located a -> a
unLoc (L _ e) = e

located :: HasLocation l =>  l -> a -> Located a
located ss x = L (locSpan ss) x

-- Monadic declarations
class Monad m => MonadLoc m where
    getLoc :: m Loc
    getLocSpan :: m LocSpan
    getLocSpan = getLoc >>= return . locSpan
    getLoc = getLocSpan >>= return . loc

class MonadLoc m => MonadSetLoc m where
    withLoc :: Loc -> m a -> m a
    withLocSpan :: LocSpan -> m a -> m a
    withLoc l a = withLocSpan (locSpan l) a
    withLocSpan ls a = withLoc (loc ls) a

withLocation :: (HasLocation l, MonadSetLoc m) => l -> m a -> m a
withLocation l a = withLocSpan (locSpan l) a

-- discard location information
instance MonadLoc Identity where
    getLoc = return mempty

instance MonadSetLoc Identity where
    withLoc _ a = a


-}
