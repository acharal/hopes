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
 - A main module for testing
 -}


module Main where

import Basic
import Error
import Pretty
import Syntax
import Parser
import Prepr
import TcUtils
import TypeCheck
import Types
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Pos

import Data.Maybe


--- TESTING FILE PROCESSING ---
content file = do 
    let fileFull = "../../pl/examples/" ++ file ++".pl" 
    (a,b) <- parseHopes2 fileFull
    return a
 
testParse = testGen id 

testPre = testGen progToGroupDag 

testTc verbatim file = do
    snt <- content file 
    case (runIdentity $ runTc initTcEnv $ progToGroupDag snt) of
        Left msgs -> mapM_ (putStrLn . show . ppr) (fst msgs)
        Right dag -> do 
            when verbatim $ mapM_ (putStrLn.show) (tcOutSyntax dag)
            mapM_ (putStrLn.show) (tcOutPreds dag)

testGen test file  = do
    snt <- content file
    mapM_ (putStrLn . show) (test snt)

simple   = "simple"
aleph    = "aleph"
testFile = "test"
prelude  = "mini-prelude"

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
testGenReader :: Show a => Tc Identity PosSpan a -> IO ()
testGenReader testReader = 
    putStrLn $ show output
        where (Right output) = runIdentity $ runErrorT $ evalStateT ( runReaderT testReader emptyTcEnv) emptyTcState  

testArity :: Tc Identity PosSpan [PiType]

testArity  = do
    pi1 <- typeOfArity 2
    pi2 <- typeOfArity 0
    pi3 <- typeOfArity 12
    return [pi1, pi2, pi3]

testVars :: Tc Identity PosSpan [Either Alpha Phi]
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
testEmptySt :: Show a => Tc Identity PosSpan a -> Tc Identity PosSpan a
testEmptySt m = do
    addExist ("NOTSEEN1") (Rho_i)
    addExist ("NOTSEEN2") (Rho_var (Alpha "notSeen2"))
    
    newAlpha
    newPhi

    addConstraint Rho_i (Rho_var $ Alpha "NOTSEENCON") bogusExp
    
    out <-  withEnvVars [("NOTSEENENV", Rho_i)] $ withEmptyState m
    
    
    return out



testNoEnvVars :: Show a => Tc Identity PosSpan a -> Tc Identity PosSpan a
testNoEnvVars m = do
    addExist "NOTEXIST1"  Rho_i 
    out <- withEnvVars [("NOTSEENENV", Rho_i)] $ withNoEnvVars m
    return out


testEnvVars :: Tc Identity PosSpan [RhoSig Symbol]
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


testExist :: Tc Identity PosSpan [RhoSig Symbol]
testExist = do
    addExist ("hello") (Rho_i)
    addExist ("world") (Rho_var (Alpha "a"))
    ex <- gets exists
    return ex

testConstraint :: Tc Identity PosSpan [Constraint PosSpan]
testConstraint = do
    addConstraint Rho_i (Rho_var $ Alpha "a") bogusExp
    addConstraint Rho_i (Rho_var $ Alpha "b") bogusExp
    con <- gets cnts
    return con




bogusInfo = typed Rho_i bogusSpan
bogusExp = SExpr_number (typed Rho_i bogusSpan) (Left 0)



