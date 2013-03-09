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

import Language.Hopl (clauseHead, clauses)
import Types (unTyp, findTySig)
import Lang (liftSym)

import Data.List(find,nub,isPrefixOf)
import Data.Char(isSpace)
import Control.Monad.Error (catchError)
import Control.Monad.State (lift, liftIO, gets)

import System.Console.Haskeline (getInputLine, setComplete, defaultSettings, completeWord, simpleCompletion, InputT, runInputT)
import System.Exit(exitWith, ExitCode(..))
import System.Console.GetOpt

import Driver (refute, consultFile)
import HopesIO
import Pretty

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


data Command =
      CRefute    String
    | CConsult   FilePath
    | CShowType  String
    | CShowDef   (Maybe String)
    | CHalt

data CommandDesc a =
    Command {
        short :: String,
        argDescr :: ArgDescr a
    }

mkCom :: CommandDesc a -> String -> a
mkCom c s =
    case argDescr c of
        NoArg a -> a
        ReqArg f _ ->  f s
        OptArg f _ ->  f (Just s)

userCommands =
 [ Command ['c','l'] (ReqArg CConsult "FILE")
 , Command ['t']     (ReqArg CShowType "SYMBOL") 
 , Command ['q']     (NoArg  CHalt)
 , Command ['p']     (OptArg CShowDef "PREDICATE")
 ]

dispatch com =
    case com of
        CRefute s   -> refute s
        CConsult f  -> consultFile f
        CShowType p -> do
            env <- gets currentEnv
            case findTySig (liftSym p) env of
                Nothing    -> fail "undefined symbol"
                Just tysig -> liftIO $ pprint tysig
        CShowDef maybe_p -> do
            src <- gets kb
            case maybe_p of
                Nothing -> liftIO $ pprint src
                Just p  -> do
                    let cl = filter (\c -> clauseHead c == liftSym p) (clauses src)
                    liftIO $ pprint $ vcat (map ppr cl)
        CHalt -> liftIO $ bye "Leaving..."

bye s     = putStrLn s >> exitWith ExitSuccess
