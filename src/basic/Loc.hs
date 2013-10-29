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

module Loc where

import Pretty
import Data.Monoid (Monoid(..))
import Control.Monad.Identity


type Line = Int
type Col  = Int

data Loc = Loc {
    locFile   :: !FilePath,
    locLine   :: !Line,
    locOffset :: !Col }
 deriving Eq

data LocSpan = 
      LocSpan Loc Loc
    | OneLineSpan   FilePath Line Col Col
    | MultiLineSpan FilePath Line Col Line Col
  deriving Eq

bogusLoc  = Loc "bogus" (-1) (-1)
bogusSpan = LocSpan bogusLoc bogusLoc

spanBegin (OneLineSpan f l c1 c2)       = Loc f l c1
spanBegin (MultiLineSpan f l1 c1 l2 c2) = Loc f l1 c1
spanBegin (LocSpan l1 l2)               = l1

spanEnd (OneLineSpan f l c1 c2)         = Loc f l c2
spanEnd (MultiLineSpan f l1 c1 l2 c2)   = Loc f l2 c2
spanEnd (LocSpan l1 l2)                 = l2

mkSpan :: (HasLocation a, HasLocation b) => a -> b -> LocSpan
mkSpan l1 l2 =
    let locl1 = loc l1 
        locl2 = loc l2
    in if locFile locl1  == locFile locl2 then
          if locLine locl1 == locLine locl2 then
               OneLineSpan (locFile locl1) (locLine locl1) (locOffset locl1) (locOffset locl2)
          else
               MultiLineSpan (locFile locl1) (locLine locl1) (locOffset locl1) (locLine locl2) (locOffset locl2)
       else
          LocSpan locl1 locl2


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


instance Show LocSpan where
    show = show . ppr

class HasLocation a where
    loc :: a ->  Loc
    locSpan :: a -> LocSpan
    locSpan x = mkSpan s s where s = loc x
    loc x = spanBegin (locSpan x)

instance HasLocation Loc where
    loc x = x

instance HasLocation LocSpan where
    locSpan x = x

instance (HasLocation a, HasLocation b) => HasLocation (a, b) where
    locSpan (a, b) = locSpan a `mappend` locSpan b

instance HasLocation a => HasLocation [a] where
    loc xs = mconcat (map loc xs)
    locSpan xs = mconcat (map locSpan xs)

data Located a = L LocSpan a deriving Eq

instance HasLocation (Located a) where
    locSpan (L l _) = l

instance Functor Located where
    fmap f (L l x) = L l (f x)

instance Monoid Loc where
    mempty = bogusLoc
    mappend a b
        | a == bogusLoc = b
        | otherwise = a

instance Monoid LocSpan where
    mempty = bogusSpan
    mappend a b
        | a == bogusSpan = b
        | otherwise = mkSpan (spanBegin (locSpan a)) (spanEnd (locSpan b))

unLoc :: Located a -> a
unLoc (L _ e) = e

located :: HasLocation l =>  l -> a -> Located a
located ss x = L (locSpan ss) x

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

