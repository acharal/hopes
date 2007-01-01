module Loc where

import Numeric (showInt)
import Pretty

type Line = Int
type Col  = Int

data Loc = Loc {
    locFile   :: !String,
    locLine   :: !Line,
    locOffset :: !Col }
 deriving Eq

data LocSpan = 
      OneLineSpan   String Line Col Col
    | MultiLineSpan String Line Col Line Col
    | UselessSpan Loc Loc
  deriving (Eq, Show)

data Located a = L LocSpan a deriving Show

instance Show Loc where
    showsPrec n (Loc f l o) = 
        (showString f).(showChar ':').
        (showInt l).(showChar ':').
        (showInt o)

unLoc :: Located a -> a
unLoc (L _ e) = e

getLoc :: Located a -> LocSpan
getLoc (L l _) = l

mkLoc :: Loc -> Loc -> a -> Located a
mkLoc l1 l2 a = L (mkSpan l1 l2) a

mkSpan :: Loc -> Loc -> LocSpan
mkSpan l1 l2 =
    if locFile l1  == locFile l2 then
        if locLine l1 == locLine l2 then
            OneLineSpan (locFile l1) (locLine l1) (locOffset l1) (locOffset l2)
        else
            MultiLineSpan (locFile l1) (locLine l1) (locOffset l1) (locLine l2) (locOffset l2)
    else
        UselessSpan l1 l2

spanBegin (OneLineSpan f l c1 c2)       = Loc f l c1
spanBegin (MultiLineSpan f l1 c1 l2 c2) = Loc f l1 c1
spanBegin (UselessSpan l1 l2)           = l1
spanEnd (OneLineSpan f l c1 c2)         = Loc f l c2
spanEnd (MultiLineSpan f l1 c1 l2 c2)   = Loc f l2 c2
spanEnd (UselessSpan l1 l2)             = l2

combLoc :: Located a -> Located b -> LocSpan
combLoc a b = combSpan (getLoc a) (getLoc b)

combSpan :: LocSpan -> LocSpan -> LocSpan
combSpan sp1 sp2 = mkSpan (spanBegin sp1) (spanEnd sp2)

instance Pretty Loc where
    ppr (Loc f l c) = hcat $ punctuate colon [ text f, int l, int c ]

instance Pretty LocSpan where
    ppr (OneLineSpan f l c1 c2) =
        hcat $ punctuate colon [ text f, int l, int c1 <> char '-' <> int c2 ]
    ppr (MultiLineSpan f l1 c1 l2 c2) = 
        hcat $ punctuate colon [ text f, ppr_par l1 c1 <> char '-' <> ppr_par l2 c2 ]
        where ppr_par l c = parens (int l <> comma <> int c)
    ppr (UselessSpan l1 l2) = ppr l1 <> char '-' <> ppr l2
