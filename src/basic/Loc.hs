{-# OPTIONS -fglasgow-exts #-}
module Loc where


import Data.Monoid
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

mkSpan :: Loc -> Loc -> LocSpan
mkSpan l1 l2 =
    if locFile l1  == locFile l2 then
        if locLine l1 == locLine l2 then
            OneLineSpan (locFile l1) (locLine l1) (locOffset l1) (locOffset l2)
        else
            MultiLineSpan (locFile l1) (locLine l1) (locOffset l1) (locLine l2) (locOffset l2)
    else
        LocSpan l1 l2

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

