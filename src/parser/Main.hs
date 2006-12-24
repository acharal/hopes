module Main where

import IO
import System
import System.IO
import Parser
import ParseUtils
import Control.Monad.Identity

import Utils


main = do
    (a:_) <- getArgs
    h <- openFile a ReadMode
    c <- hGetContents h
    case runIdentity $ runP parseSrc (mkState c) of
	Right (p,s) -> pprint p
	Left e -> pprint e
