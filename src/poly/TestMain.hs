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



{-
 - A main module for testing/ invoking the type checker
 -}


module TestMain where

import Basic
import Syntax
import Types
import TcUtils
import TypeCheck
import Prepr (progToGroupDag)
import Loc (bogusSpan, LocSpan)
import Pretty (ppr, render)
import Parser (parseHopes2)
import Frontend (loadSource, printDef, printType)

import Data.Maybe(fromJust)
import System.Environment(getArgs)
import System.Exit (exitFailure)
import Control.Monad.Identity (runIdentity, Identity)



-- Calling main' inputfile verbose from interactive:
--     Parses operator file
--     Parses and typeChecks builtins file
--     Parses and typeChecks inputFile
--     If verbose, prints syntax tree with annotations
--     Prints either error or typechecked predicates
main' verbose inputFile = do
    (src, tyenv) <- loadSource inputFile

    when verbose $ mapM_ printDef src

    mapM_ printType tyenv


-- Invoking polyhopes program:
--     Parses operator file
--     Parces and typeChecks builtins file
--     Parces and typeChecks first command line argument
--     Prints either error or typechecked predicates
-- Fails if (length args =\= 1)
main = do
    args <- getArgs
    when (null args) $ do
        putStrLn $ "Error: no file given"
        exitFailure
    when (length args > 1) $ do 
        putStrLn $ "Error: Only one file allowed"
        exitFailure
    main' True (head args)

-- Wrapper for runTc
-- runTc' env prog = runIdentity $ runTc env (progToGroupDag prog)

simple   = "../../pl/examples/simple.pl"
aleph    = "../../pl/examples/aleph.pl"
testFile = "../../pl/examples/test.pl"
prelude  = "../../pl/examples/mini-prelude.pl"




------------- Various tests --------------



-- Old typeCheck tester
testTc verbose file = do
    bui <- content "../../pl/builtins.pl" 
    let Right buis = runIdentity $ runTc initTcEnv $ progToGroupDag bui
    fl <- content file
    case (runIdentity $ runTc ( addPredsToEnv initTcEnv (tcOutPreds buis) ) $ progToGroupDag fl) of
        Left msgs -> mapM_ (putStrLn . show . ppr) (fst msgs)
        Right dag -> do
            when verbose $ mapM_ (putStrLn.show) (tcOutSyntax dag)
            mapM_ (putStrLn.show) (tcOutPreds dag)





--- TESTING FILE PROCESSING ---
content file = do
    (a,b) <- parseHopes2 file
    return a
 
testParse = testGen id 

testPretty = testGen ( map (render . ppr) )

testPre = testGen progToGroupDag 

testGen test file  = do
    snt <- content file
    mapM_ (putStrLn . show) (test snt)


--- TESTING GENERAL FUNCTIONS ---
testFlatten = testGen $ 
    concatMap ( flatten . snd . fromJust . clBody .
                (\(SSent_clause  cl) -> cl) 
              )

testFree1 rho = freeAlphas rho
testFree2 rho = freePhis   rho

type1 = Rho_pi (Pi_fun [Rho_pi $ Pi_fun [Rho_var $ Alpha "a1"] Pi_o] (Pi_fun [Rho_var $ Alpha "a1"] Pi_o))

type2 = Rho_pi (Pi_fun [Rho_pi $ Pi_fun [Rho_pi $ Pi_var $ Phi "phi1"] Pi_o] (Pi_fun [Rho_var $ Alpha "a2"] Pi_o))


expr1 = SExpr_const (typed type1 ()) (Const (typed type1 ()) "con") True Nothing 0

subst = substAlpha (Alpha "a1") (Rho_var $ Alpha "subst")

--testFresh tp = let Right r = runIdentity $ runErrorT $ evalStateT (runReaderT (freshen tp) emptyTcEnv) emptyTcState in r

--- TESTING TC MONAD ---
testGenReader :: Show a => Tc Identity LocSpan a -> IO ()
testGenReader testReader = 
    putStrLn $ show output
        where (Right output) = runIdentity $ runErrorT $ evalStateT ( runReaderT testReader emptyTcEnv) emptyTcState  

testArity :: Tc Identity LocSpan [PiType]

testArity  = do
    pi1 <- typeOfArity 2
    pi2 <- typeOfArity 0
    pi3 <- typeOfArity 12
    return [pi1, pi2, pi3]

testVars :: Tc Identity LocSpan [Either Alpha Phi]
testVars = do
    al1 <- newAlpha
    al2 <- newAlpha
    phi1 <- newPhi
    al3 <- newAlpha
    als <- newAlphas 2
    phis <- newPhis 4
    return $ [Left al1, Left al2, Right phi1, Left al3] ++
        map Left als ++ map Right phis



-- tested with: testExist
--              testFreshen 
--              testConstraint
--              testEnvVars
testEmptySt :: Show a => Tc Identity LocSpan a -> Tc Identity LocSpan a
testEmptySt m = do
    addExist ("NOTSEEN1") (Rho_i)
    addExist ("NOTSEEN2") (Rho_var (Alpha "notSeen2"))
    
    newAlpha
    newPhi

    addConstraint Rho_i (Rho_var $ Alpha "NOTSEENCON") bogusExp
    
    out <-  withEnvVars [("NOTSEENENV", Rho_i)] $ withEmptyState m
    
    
    return out



testNoEnvVars :: Show a => Tc Identity LocSpan a -> Tc Identity LocSpan a
testNoEnvVars m = do
    addExist "NOTEXIST1"  Rho_i 
    out <- withEnvVars [("NOTSEENENV", Rho_i)] $ withNoEnvVars m
    return out


testEnvVars :: Tc Identity LocSpan [RhoSig Symbol]
testEnvVars = do
    vars <- asks rhoSigs
    return vars


testFresh tp = do
    tp' <- freshen tp 
    return tp'

toFreshen = Poly_gen [Alpha "s", Alpha "s2"] [Phi "phi"] $
    Pi_fun  [Rho_pi $ Pi_fun [Rho_var $ Alpha "s", Rho_var $ Alpha "s2"] Pi_o] $
            Pi_fun [Rho_var $ Alpha "s", Rho_pi $ Pi_var $ Phi "phi"] 
                   Pi_o


testExist :: Tc Identity LocSpan [RhoSig Symbol]
testExist = do
    addExist ("hello") (Rho_i)
    addExist ("world") (Rho_var (Alpha "a"))
    ex <- gets exists
    return ex

testConstraint :: Tc Identity LocSpan [Constraint LocSpan]
testConstraint = do
    addConstraint Rho_i (Rho_var $ Alpha "a") bogusExp
    addConstraint Rho_i (Rho_var $ Alpha "b") bogusExp
    con <- gets cnts
    return con




bogusInfo = typed Rho_i bogusSpan
bogusExp = SExpr_number (typed Rho_i bogusSpan) (Left 0)



