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
import Control.Monad.State (lift, liftIO, gets, modify)

import System.Console.Haskeline (getInputLine, setComplete, defaultSettings, completeWord, completeFilename, simpleCompletion, InputT, runInputT)
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
        where defaultShellSettings = setComplete completeFilename defaultSettings

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
{-
completePredicates = -- completeQuotedWord Nothing "\'" listPredicates $
                        completeWord Nothing "" listPredicates

listPredicates s =
        let predicates kb = nub $ map (show.ppr.unTyp.clauseHead) $ clauses kb
        in do
             k <- gets kb
             return $ map simpleCompletion $ filter (isPrefixOf s) $ predicates k
-}

data Command =
      CRefute    String
    | CConsult   FilePath
    | CShowType  String
    | CShowDef   (Maybe String)
    | CDebugOnOff (Maybe String)
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
 , Command ['d']     (OptArg CDebugOnOff "on/off")
 ]


dispatch (CRefute s) = refute s
dispatch command = dispatch' command
    where dispatch' (CConsult f)  = consultFile f
          dispatch' (CShowType p) = showType p
          dispatch' (CShowDef  p) = fail "showDef"-- showDef p
          dispatch' (CDebugOnOff s) = debugOnOff s
          dispatch' (CHalt) = halt


toggleDebug Nothing  = modify (\s -> s{debugFlag = not (debugFlag s)})
toggleDebug (Just "on")  = modify (\s -> s{debugFlag = True})
toggleDebug (Just "off") = modify (\s -> s{debugFlag = False})
toggleDebug (Just _) = toggleDebug Nothing

printDebug = let 
       onoff True  = "on"
       onoff False = "off"   
    in do
        d <- gets debugFlag
        writeInfo ("debug is " ++ onoff d)

debugOnOff i = toggleDebug i >> printDebug

{-
showDef Nothing = do
    src <- gets kb
    liftIO $ pprint src
showDef (Just p) = do
    src <- gets kb
    let cl = filter (\c -> clauseHead c == liftSym p) (clauses src)
    liftIO $ pprint $ vcat (map ppr cl)
-}

showType p = do
    env <- gets currentEnv 
    case findTySig (liftSym p) env of
        Nothing -> fail "undefined symbol"
        Just tysig -> liftIO $ pprint tysig

halt = bye "halting ..."

bye s = do
    writeInfo s 
    liftIO $ exitWith ExitSuccess

writeInfo s = liftIO $ comm s
    where comm s = putStr "% "  >> putStrLn s

