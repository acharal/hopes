module Loc where

import Numeric (showInt)

type Line = Int
type Col  = Int

data Loc = Loc {
    locFile   :: !String,
    locLine   :: !Line,
    locOffset :: !Col }
 deriving Eq

instance Show Loc where
    showsPrec n (Loc f l o) = (showString f).(showChar ':').(showInt l).(showChar ':').(showInt o)

data LocSpan = LocSpan {
    spanBegin :: Loc,
    spanEnd   :: Loc }
 deriving (Eq,Show)

data Located a = L LocSpan a
 deriving Show

unLoc :: Located a -> a
unLoc (L _ e) = e

getLoc :: Located a -> LocSpan
getLoc (L l _) = l

startOfFile :: String -> Loc
startOfFile filename = Loc filename 1 0

mkSpan :: Loc -> Loc -> LocSpan
mkSpan = LocSpan

mkLoc :: Loc -> Loc -> a -> Located a
mkLoc l1 l2 a = L (mkSpan l1 l2) a

combLoc :: Located a -> Located b -> LocSpan
combLoc a b = combSpans (getLoc a) (getLoc b)

combSpans :: LocSpan -> LocSpan -> LocSpan
combSpans sp1 sp2 = mkSpan (spanBegin sp1) (spanEnd sp2)