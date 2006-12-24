module ParseUtils where

import Utils
import Control.Monad.State
import Control.Monad.Identity

type StringBuffer = String

data Token =
      TKoparen
    | TKcparen
    | TKgets
    | TKdot
    | TKcomma
    | TKvert
    | TKobrak
    | TKcbrak
    | TKocurly
    | TKccurly
    | TKwild
    | TKcolcol
    | TKsemi
    | TKcut
    | TKslash
    | TKbslash
    | TKarrow
    | TKid String
    | TKEOF
   deriving Eq

instance Show Token where
    showsPrec n (TKoparen) = showString "("
    showsPrec n (TKcparen) = showString ")"
    showsPrec n (TKgets)   = showString ":-"
    showsPrec n (TKdot)    = showString "."
    showsPrec n (TKcomma)  = showString ","
    showsPrec n (TKvert)   = showString "|"
    showsPrec n (TKobrak)  = showString "["
    showsPrec n (TKcbrak)  = showString "]"
    showsPrec n (TKocurly) = showString "{"
    showsPrec n (TKccurly) = showString "}"
    showsPrec n (TKwild)   = showString "_"
    showsPrec n (TKcolcol) = showString "::"
    showsPrec n (TKsemi)   = showString ";"
    showsPrec n (TKcut)    = showString "!"
    showsPrec n (TKslash)  = showString "/"
    showsPrec n (TKbslash) = showString "\\"
    showsPrec n (TKarrow)  = showString "->"
    showsPrec n (TKid s)   = showString s

type LTok = Located Token

mkLTk = mkLoc

type Parser = StateT ParseState (ErrorT HopeError Identity)
--type Parser m = StateT ParseState (ErrorT HopeError m)

data ParseState = PState {
    buffer   :: StringBuffer,
    --last_loc :: Loc,
    last_tok :: Located Token,
    cur_tok  :: Located Token,
    loc      :: Loc }
    deriving Show

mkState :: String -> ParseState
mkState inp = PState inp tok tok loc
    where loc = Loc "unknown" 1 1
          tok = undefined


getSrcBuf :: Parser StringBuffer
getSrcBuf = get >>= return.buffer

setSrcBuf :: StringBuffer -> Parser ()
setSrcBuf inp = modify (\s -> s{buffer=inp})

getSrcLoc :: Parser Loc
getSrcLoc = get >>= return.loc

setSrcLoc :: Loc -> Parser ()
setSrcLoc l = modify (\s -> s{loc=l})

{-
setPSrcLoc :: Loc -> Parser ()
setPSrcLoc l = modify (\s -> s{last_loc=l})

getPSrcLoc :: Parser Loc
getPSrcLoc = get >>= return.last_loc
-}

setLastTok :: Located Token -> Parser ()
setLastTok t = modify (\s -> s{cur_tok=t, last_tok = (cur_tok s)})

getLastTok :: Parser (Located Token)
getLastTok = get >>= return.last_tok

getTok :: Parser (Located Token)
getTok = get >>= return.cur_tok

runP p = runErrorT.(runStateT p)

{-

prog <- parseProg `catchError` (\e -> print e)

-}
