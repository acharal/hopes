--  Copyright (C) 2013 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--                     Emmanouil Koukoutos   <manoskouk@softlab.ntua.gr>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Frontend where

import Pretty
import Syntax
import Parser (runParser, parseSrc, emptyParseState, getState)
import Prepr (progToGroupDag)
import TcUtils (initTcEnv, addPredsToEnv)
import TypeCheck (runTc, tcOutPreds, tcOutSyntax)
import Core
import Desugar (desugarDef)

import Control.Monad.IO.Class (liftIO)

import Text.Parsec.Error (ParseError)

instance Pretty ParseError where
    ppr err = text ("Parse error at " ++ show err)

-- Wrapper function for runP. Returns both parsed program and final state
-- Parameters : initial state, input file.
parseFile st inputFile = do
    input <- liftIO $ readFile inputFile
    runParser p' st inputFile input
    where p' = do 
            sents <- parseSrc
            st' <- getState
            return (sents, st')

-- Wrapper for runTc
runTc' env prog = runTc env (progToGroupDag prog)

processMsgs (errs, warns) = do
    liftIO $ print $ vcat (map ppr errs)
    fail "errors occured"

printType (sig, tp) = do
    liftIO $ putStr $ show (ppr sig)
    liftIO $ putStrLn $ " :: " ++ show tp

printDef def = liftIO $ print $ ppr def


loadSource inputFile = do
    Right (_, opTable)   <- parseFile emptyParseState "../../pl/op.pl"
    -- Parse builtins
    Right (buis, buisST) <- parseFile opTable "../../pl/builtins.pl"
    -- TypeCheck builtins
    Right buis' <- runTc' initTcEnv buis
    -- Parse input
    maybeParsed <- parseFile buisST inputFile

    case maybeParsed of 
        Left err -> processMsgs ([err], [])        
        Right (parsed, _) -> do
            let env = addPredsToEnv initTcEnv (tcOutPreds buis')
            typeChecked <- runTc' env parsed
            case typeChecked of
                -- Type error
                Left errors -> processMsgs errors
                -- TypeCheck successful
                Right dag -> 
                    return (concatMap (map desugarDef) (tcOutSyntax dag), tcOutPreds dag)

