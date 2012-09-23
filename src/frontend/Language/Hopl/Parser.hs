--  Copyright (C) 2006-2012 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Language.Hopl.Parser () where

import Parser ()

{- 
--loadSource :: String -> IO (Maybe (Prog, TypeEnv), Messages)
loadSource file = do
    parsed       <- liftIO $ parseFromFile file parseSrc
    (wellformed, msgs) <- liftIO $ runTc $ wfp parsed
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
    (tcres, msgs) <- liftIO $ runTc $ withSig parsed_goal $
                              withTypeEnv env $ wfg parsed_goal
    case tcres of
        Just (tcgoal, env') -> do
            cg <- runDesugarT $ desugarGoal (tcgoal, env')
            return cg
        Nothing -> processMsgs msgs

parseFromFile fname parser = do
    file     <- liftIO $ openFile fname ReadMode
    content  <- liftIO $ hGetContents file
    parsed   <- runParser $ fromFile fname $ withInput content $ parser
    case parsed of
        Right (ast,s) -> return ast
        Left   msgs -> processMsgs msgs
-}
