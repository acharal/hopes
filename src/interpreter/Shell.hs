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

import Driver (mkCom, dispatch, runDriverM, Command(..), CommandDesc(..), HopeEnv(..), userCommands)
import Language.Hopl (clauseHead, clauses)
import Types (unTyp)
import Pretty (ppr)
import Data.List(find,nub,isPrefixOf)
import Data.Char(isSpace)
import Control.Monad.Error (catchError)
import Control.Monad.State (lift, liftIO, gets)

import System.Console.Haskeline (getInputLine, setComplete, defaultSettings, completeWord, simpleCompletion, InputT, runInputT)


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


