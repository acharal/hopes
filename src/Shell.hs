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

module Shell(runInteractive, Command(..)) where

import List(find,nub)
import Data.List(isPrefixOf)
import Char(isSpace)
import Driver
import KnowledgeBase
import Hopl
import Types
import Pretty
import Control.Monad.Error
import Control.Monad.State

import System.Console.Haskeline

-- import Control.Monad.State

trim :: String -> String
trim xs = dropWhile (isSpace) $ reverse $ dropWhile (isSpace) $ reverse xs

parseCommand' (':':x:xs) =
    case find (\c -> any (x==) (short c)) userCommands of
        Nothing -> fail "Unknown command"
        Just s  -> return $ mkCom s (trim xs)

parseCommand' str = return $ CRefute str

initializeShell = return ()

readline  = getInputLine

type ShellT = InputT

runShell m = runInputT defaultShellSettings (initializeShell >> m)
        where defaultShellSettings = setComplete completePredicates defaultSettings

prompt = readline promptStr
    where promptStr = "-? "

getCommand = do
    maybe_inp <- prompt
    case maybe_inp of
        Nothing -> getCommand
        Just "" -> getCommand
        Just str -> do
            parseCommand' str --`catchError`
--                        (\e -> (liftIO $ print e) >> getCommand)

runLoopM = do
        command <- getCommand
        lift $ dispatch command `catchError` (\e -> (liftIO $ print e))
        runLoopM

runInteractiveM commands = runShell runLoopM

runInteractive commands = runDriverM $ runInteractiveM commands

-- completion

completePredicates = -- completeQuotedWord Nothing "\'" listPredicates $ 
                        completeWord Nothing "" listPredicates

listPredicates s = 
        let predicates kb = nub $ map (show.ppr.unTyp.clauseHead) $ clauses kb 
        in do
             k <- gets kb
             return $ map simpleCompletion $ filter (isPrefixOf s) $ predicates k


