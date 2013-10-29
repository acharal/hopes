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


module Frontend (loadSource, loadGoal) where

import Tc (runTc, withSig, withTypeEnv)
import WellForm (wfp, wfg)
import Desugar (runDesugarT, desugarGoal, desugarSrc)
import Parser (runParser, withInput, parseSrc, parseGoal, fromFile)
import Pretty
import Language.Hopl (KnowledgeBase(..))
import HoplToCore (kbtoProgram, hopltoCoreGoal)

import Control.Monad.IO.Class (liftIO)

parseFromFile fname parser = do
    content  <- liftIO $ readFile fname
    parsed   <- runParser $ fromFile fname $ withInput content $ parser
    case parsed of
        Right (ast,s) -> return ast
        Left   msgs   -> processMsgs msgs

processMsgs (errs, warns) = do
    liftIO $ print $ vcat (map ppr errs)
    fail "errors occured"

--loadSource :: String -> IO (Maybe (Prog, TypeEnv), Messages)

loadSource file = do
    parsed <- liftIO $ parseFromFile file parseSrc
    (wellformed, msgs) <- runTc (wfp parsed)
    case wellformed of
        Just (wfprog, env) -> do
                cp <- runDesugarT $ desugarSrc (wfprog, env)
                return (kbtoProgram (KB cp), env)
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
