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

module Main where

import Paths_hopes(version)
import System.Exit(exitWith, ExitCode(..))
import System.Environment(getArgs, getProgName)
import System.Console.GetOpt
import Data.Version(showVersion)
-- import Driver
import Shell

data CLIFlag =
      CliConsultFile String
    | CliRunFile String
    | CliRunGoal String
    | CliShowHelp
    | CliShowVersion
   deriving Eq

main :: IO ()
main = do
    args <- getArgs
    mainWith args

constArgs = []

mainWith args =
    case getOpt Permute argInfo (constArgs ++ args) of
        (cli,_,[]) | CliShowVersion `elem` cli ->
            bye copyright
        (cli,_,[]) | CliShowHelp `elem` cli -> do
            prog <- getProgName
            bye (usageInfo (usageHeader prog) argInfo)
        (cli,_,[]) ->
            runInteractive $ toCommand cli
        (_,_,errors) -> do
            prog <- getProgName
            die (concat errors ++
                 usageInfo (usageHeader prog) argInfo)


copyright = unlines [
  "HOPES interpreter, version " ++ showVersion version,
  "Copyright (c) 2006-2008 Angelos Charalambidis"
  ] 

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die s = putStr s >> exitWith (ExitFailure 1)

usageHeader :: String -> String
usageHeader prog = "Usage: " ++ prog ++ " [OPTIONS...]\n"


banner :: String
banner = copyright


argInfo :: [OptDescr CLIFlag]
argInfo = [
    Option ['l'] ["load"] (ReqArg CliConsultFile "FILE")
        "consult prolog file FILE before entering top level",
    Option ['L'] [] (ReqArg CliRunFile "FILE")
        "run prolog file FILE and exit",
    Option ['g'] [] (ReqArg CliRunGoal "GOAL")
        "run the goal GOAL before entering top level",
    Option ['?', 'h'] [] (NoArg CliShowHelp)
        "display this help and exit",
    Option ['V'] ["version"] (NoArg CliShowVersion)
        "output version information and exit"
    ]

toCommand :: [CLIFlag] -> [Command]
toCommand cli = collectFiles cli ++ getGoal cli ++ mustHalt cli
    where collectFiles ((CliConsultFile f):r) = (CConsult f):(collectFiles r)
          collectFiles ((CliRunFile f):r)     = [CConsult f]
          collectFiles _ = []
          getGoal cli = case cli of
                            [] -> []
                            (CliRunGoal g):_ -> [CRefute (g ++ ".")]
                            (_:rcli) -> getGoal rcli
          mustHalt cli = case cli of
                            [] -> []
                            (CliRunFile _):_ -> [CHalt]
                            (_:rcli) -> mustHalt rcli

