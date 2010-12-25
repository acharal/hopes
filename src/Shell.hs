--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Shell(runShell, ShellT, getCommand, Command(..)) where

import List(find)
import Char(isSpace)
-- import Flags(Command(..), userCommands, mkCom, short)
import System.Console.GetOpt
import System.Console.Haskeline

-- import Control.Monad.State

trim :: String -> String
trim xs = dropWhile (isSpace) $ reverse $ dropWhile (isSpace) $ reverse xs


parseCommand' (':':x:xs) =
    case find (\c -> any (x==) (short c)) userCommands of
        Nothing -> fail "Unknown command"
        Just s  -> return $ mkCom s (trim xs)

parseCommand' str = return $ CRefute str


data Command =
      CRefute    String
    | CConsult   FilePath
    | CShowType  String
    | CShowDef   (Maybe String)
    | CHalt
    | CBuildin   String [String]

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


--initializeShell = do
    -- liftIO $ Readline.setCatchSignals False
--    liftIO $ Readline.initialize
--    liftIO $ Readline.setCompletionEntryFunction (Just (Readline.filenameCompletionFunction))

--readline = liftIO . Readline.readline
--addHistory = liftIO . Readline.addHistory

initializeShell = return ()

readline  = getInputLine 

-- addHistory s = liftIO $ return ()

type ShellT = InputT

runShell m = runInputT defaultShellSettings (initializeShell >> m)
        where defaultShellSettings = defaultSettings

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


-- completion

-- completeNone :: String -> HopesIO [String]
-- completeNone _ = return []
