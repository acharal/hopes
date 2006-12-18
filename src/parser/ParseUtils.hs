module ParseUtils where

import Loc
import ErrUtils

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

type StringBuffer = String

--type Parser = ErrorT ErrMsg (StateT ParseState Identity)
type Parser = StateT ParseState (ErrorT ErrMsg Identity)

data ParseState = PState {
    buffer   :: StringBuffer,
    last_loc :: Loc,
    loc      :: Loc }
    deriving Show

mkState :: String -> ParseState
mkState inp = PState inp loc loc
    where loc = Loc "unknown" 1 0


getSrcBuf :: Parser StringBuffer
getSrcBuf = do
    st <- get
    return (buffer st)


setSrcBuf :: StringBuffer -> Parser ()
setSrcBuf inp = do
    st <- get
    put st{buffer=inp}

getSrcLoc :: Parser Loc
getSrcLoc = do
    st <- get
    return (loc st)

setSrcLoc :: Loc -> Parser ()
setSrcLoc l = do
    st <- get
    put st{loc=l}

setPSrcLoc :: Loc -> Parser ()
setPSrcLoc l = do
    st <- get
    put st{last_loc=l}

getPSrcLoc :: Parser Loc
getPSrcLoc = do
    st <- get
    return (last_loc st)


runP p = runErrorT.(runStateT p)

{-

prog <- parseProg `catchError` (\e -> print e)

-}
