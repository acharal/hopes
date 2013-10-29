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

import Infer
import Pretty

import Frontend
import Debugger
import HopesIO

import Control.Monad.State (gets, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import System.IO (stdin, hSetBuffering, BufferMode(..))

consultFile f = do
    (src, env) <- loadSource f
    modify (\s -> s{ p = src, 
                     currentEnv = env})
    liftIO $ putStrLn ("% consulted " ++ show f ++ "")

sayYes    = putStrLn "Yes"
sayNo     = putStrLn "No"

refute s = do
    env  <- gets currentEnv
    goal <- loadGoal s env
    consumeSolutions $ attachDebugger $ prove goal

-- consumeSolutions :: Infer a b -> HopesIO ()
consumeSolutions i = do
    src    <- gets p
    result <- infer src i

    case result of
        Nothing -> 
            liftIO $ sayNo
        Just (a, rest) -> do
            liftIO $ sayYes
            liftIO $ print $ ppr a
            liftIO $ hSetBuffering stdin NoBuffering
            c <- liftIO $ getChar
            when (c == ';') $ do
                liftIO $ putChar '\n';    
                consumeSolutions rest

