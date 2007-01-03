module Main where

import IO
import System
import System.IO
import Parser
import ParseUtils
import Control.Monad.Identity

import Utils

--import Tc
import Tc

import HpSyn

main = do
    (a:_) <- getArgs
    h <- openFile a ReadMode
    c <- hGetContents h
    case runIdentity $ runParser parseSrc (mkState c) of
	Right (p,s) -> do
            pprint p
            case runTc (tcSource p) of
                Right ((p', env),_) -> print $ ppr_env env
                Left e -> pprint $ vcat (map ppr (processMsgs e))
            return ()
	Left e ->
            pprint $ vcat (map ppr (processMsgs e))

ppr_env env = vcat (map ppr_aux env)
    where ppr_aux (v, t) = hang ((text v) <+> (text "::")) (length v + 4) (ppr t)
