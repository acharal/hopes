module Main where

import IO
import System
import System.IO
import Parser
import ParseUtils
import Control.Monad.Identity

import Utils

import Tc

import HpSyn

main = do
    (a:_) <- getArgs
    h <- openFile a ReadMode
    c <- hGetContents h
    case runIdentity $ runParser parseSrc (mkState c) of
	Right (p,s) -> do
            pprint p
            case runTc (tycheckP p) of
                (Right env,_) -> print $ ppr_env env
                (Left e,_) -> pprint e
            return ()
	Left e ->
            pprint e

ppr_env env = vcat (map ppr_aux env)
    where ppr_aux (v, t) = hang ((text v) <+> (text "::")) (length v + 4) (ppr t)