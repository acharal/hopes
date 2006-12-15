module ParseUtils where

import Loc
import ErrUtils

type StringBuffer = String

newtype Parser a = P { runP :: (ParseState -> ParseResult a) }

data ParseState = PState {
    buffer   :: StringBuffer,
    last_loc :: Loc,
    loc      :: Loc }

data ParseResult a =
     POk ParseState a
   | PFailed Loc ErrMsg

-- monadic operators on Parsers
instance Monad Parser where
    return = returnP
    (>>=) = thenP
    fail = failP

thenP :: Parser a -> (a -> Parser b) -> Parser b
P m `thenP` k = P $ \s->
   case m s of 
      POk s' a -> (runP (k a)) s'
      PFailed l e -> PFailed l e

mkState :: String -> ParseState
mkState inp = PState inp loc loc
    where loc = Loc "unknown" 1 0

returnP :: a -> Parser a
returnP a = P $ \s -> POk s a

failP :: String -> Parser a
failP msg = P $ \s -> PFailed (loc s) (errMsg msg)

getPInput :: Parser StringBuffer
getPInput = P $ \s -> POk s (buffer s)

setPInput :: StringBuffer -> Parser ()
setPInput inp = P $ \s -> POk s{buffer=inp} ()

getPLoc :: Parser Loc
getPLoc = P $ \s -> POk s (loc s)

setPLoc :: Loc -> Parser ()
setPLoc l = P $ \s -> POk s{loc=l,last_loc=(loc s)} ()


