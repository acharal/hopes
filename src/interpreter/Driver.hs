--  Copyright (C) 2006-2011 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

-- | drives the pipeline of compilation + execution
module Driver where


import Language.Hopl (KnowledgeBase(..))

import Parser (runParser, withInput, parseSrc, parseGoal, fromFile)
import Tc (runTc, withSig, withTypeEnv)
import WellForm (wfp, wfg)
import Desugar (runDesugarT, desugarGoal, desugarSrc)
import Infer
import CoreLang (kbtoProgram, hopltoCoreGoal)
import Pretty

import Control.Monad.State (gets, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import System.IO

import Debugger
import HopesIO

parseFromFile fname parser = do
    file     <- liftIO $ openFile fname ReadMode
    content  <- liftIO $ hGetContents file
    parsed   <- runParser $ fromFile fname $ withInput content $ parser
    case parsed of
        Right (ast,s) -> return ast
        Left   msgs -> processMsgs msgs

processMsgs (errs, warns) = do
    liftIO $ print $ vcat (map ppr errs)
    fail "errors occured"

--loadSource :: String -> IO (Maybe (Prog, TypeEnv), Messages)

loadSource file = do
    parsed       <- liftIO $ parseFromFile file parseSrc
    (wellformed, msgs) <- runTc (wfp parsed)
    case wellformed of
        Just (wfprog, env) -> do
                cp <- runDesugarT $ desugarSrc (wfprog, env)
                return (cp, env)
        Nothing -> processMsgs msgs

loadGoal inp env = do
    parsed_res  <- liftIO $ runParser $ withInput inp $ parseGoal
    parsed_goal <- case parsed_res of
                        Right (g,_) -> return g
                        Left msgs   -> processMsgs msgs
    (tcres, msgs) <- runTc $ withSig parsed_goal $
                              withTypeEnv env $ wfg parsed_goal
    case tcres of
        Just (tcgoal, env') -> do
            cg <- runDesugarT $ desugarGoal (tcgoal, env')
            return (hopltoCoreGoal cg)
        Nothing -> processMsgs msgs


consultFile f = do
    (src, env) <- loadSource f
    modify (\s -> s{ kb = KB src, 
                     p = (kbtoProgram (KB src)), 
                     currentEnv = env})
    liftIO $ putStrLn ("% consulted " ++ show f ++ "")

sayYes    = putStrLn "Yes"
sayNo     = putStrLn "No"

refute s = do
    src  <- gets kb
    env  <- gets currentEnv
    goal <- loadGoal s env
    consumeSolutions $ attachDebugger $ prove goal
-- consumeSolutions :: Infer a b -> HopesIO ()
consumeSolutions i = do
    src  <- gets p
    liftIO $ hSetBuffering stdin NoBuffering
    result <- infer src i
--    case runIdentity (infer src i) of
    case result of
        Nothing -> 
            liftIO $ sayNo
        Just (a, rest) -> do
            liftIO $ sayYes
            liftIO $ print $ ppr a
            c <- liftIO $ getChar
            when (c == ';') $ do
                liftIO $ putChar '\n';    
                consumeSolutions rest

