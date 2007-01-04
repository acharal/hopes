module Main where

import IO
import System
import System.IO
import Parser
import ParseUtils
import Control.Monad.Identity
import Pretty
import Err
import Hopl

import Tc

import HpSyn

main = do
    (f:_) <- getArgs
    pres  <- parseFromFile parseSrc f
    case pres of
	Right (p,s) -> do
            pprint p
            case runTc (tcSource p) of
                (Just (p', env),msg) -> do
                    print $ ppr_env env
                    print $ runSimple (simplifyProg p')
                (Nothing, msg) -> pprint $ vcat (map ppr (processMsgs msg))
            return ()
	Left msgs ->
            pprint $ vcat (map ppr (processMsgs msgs))

ppr_env env = vcat (map ppr_aux env)
    where ppr_aux (v, t) = hang ((text v) <+> (text "::")) (length v + 4) (ppr t)
