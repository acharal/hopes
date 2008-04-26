
--  Copyright (C) 2007 2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Interactive where

import Syntax
import Symbol
import Pretty
import Error
import Loc
import Types
import Parser
import WellForm
import Core
import Infer
import Hopl

import List                         (isPrefixOf, find)
import Char                         (isSpace)
import IO
import System
import System.IO
import System.Console.Readline
import System.Console.GetOpt
import Control.Monad.State

import Hope

type HopeI = StateT HopeEnv IO


data Command =
      RefuteGoal  String
    | ConsultSrc  FilePath
    | ShowType    String
    | ShowHelp
    | Quit

data CommandDesc a = Command { short :: String, argDescr :: ArgDescr a,  desc:: String }

mkCom :: CommandDesc a -> String -> a
mkCom c s = 
    case argDescr c of
        NoArg a -> a
        ReqArg f _ ->  f s
        OptArg f _ ->  f (Just s)

userCommands =
 [ Command ['c','l'] (ReqArg ConsultSrc "FILE")   "Consult a file"
 , Command ['t']     (ReqArg ShowType "SYMBOL")   "Show a type of a symbol"
 , Command ['h']     (NoArg  ShowHelp)            "Show help"
 , Command ['q']     (NoArg  Quit)                "Quit"
 ]

trim :: String -> String
trim xs = dropWhile (isSpace) $ reverse $ dropWhile (isSpace) $ reverse xs

getCommand commands (':':x:xs) = 
    case find (\c -> any (x==) (short c)) commands of
        Nothing -> fail "Unknown command"
        Just s  -> return $ mkCom s (trim xs)

getCommand commands str = return $ RefuteGoal str

{-
commandCompletionFunction env xs = goalCompletionFunction env xs
goalCompletionFunction = predicateCompletionFunction

predicateCompletionFunction env p = 
    let vs =  map show [ v | (v,_) <- (currentEnv env) ]
    in  do
        return $ filter (p `isPrefixOf`) vs
-}

runInteract act = evalStateT (interactLoop act) initEnv
    where initEnv = HEnv [] []

action (RefuteGoal s) = do
    src  <- gets consultedSrc
    env  <- gets currentEnv
    goal <- loadGoal s env
    showSolutions $ runInfer src (prove goal)

action (ConsultSrc f) = consultFile f

action (ShowType p) = do
    env <- gets currentEnv
    case findTySig (liftSym p) env of
        Nothing    -> fail "undefined symbol"
        Just tysig -> liftIO $ pprint tysig

action Quit = liftIO $ bye

action _    = fail "functionality not implemented"

consultFile :: FilePath -> HopeI ()
consultFile f = do
    (src, env) <- loadSource f
    modify (\s -> s{ consultedSrc = src, currentEnv = env})

showHelp  = putStrLn "no help"

bye       = putStrLn "Bye" >> exitWith ExitSuccess
sayYes    = putStrLn "Yes"
sayNo     = putStrLn "No"

promptStr = "-? "

interactLoop :: (Command -> HopeI ()) -> HopeI ()
interactLoop act = do
    env <- get
    liftIO $ setCompletionEntryFunction (Just (filenameCompletionFunction))
    maybe_inp <- liftIO $ readline promptStr
    case maybe_inp of
        Nothing -> interactLoop act
        Just "" -> interactLoop act
        Just str -> do
            liftIO $ addHistory str
            (getCommand userCommands str >>= act) `catchError` (\e -> (liftIO $ print e))
            interactLoop act

showSolutions []  = liftIO $ sayNo
showSolutions [s] = liftIO $ pprint s >> sayYes
showSolutions (s:sols) = do
    liftIO $ pprint s
    c <- liftIO $ getChar
    when (not $ c == 'q') $ showSolutions sols



parseFromFile :: MonadIO m => FilePath -> ParserT m a -> m a
parseFromFile fname parser = do 
    file <- liftIO $ openFile fname ReadMode
    inp  <- liftIO $ hGetContents file
    parsed_res <- runParser $ fromFile fname $ withInput inp $ parser
    case parsed_res of
        Right (p,s) -> return p
        Left   msgs -> processMsgs msgs


processMsgs (errs, warns) = do
    liftIO $ print $ vcat (map ppr errs)
    fail "errors occured"

--loadSource :: String -> IO (Maybe (Prog, TypeEnv), Messages)
loadSource file = do
    parsed_prog   <- liftIO $ parseFromFile file parseSrc
    (tcres, msgs) <- liftIO $ runTc $ wfp parsed_prog
    case tcres of
        Just (tcprog, env) -> do
                cp <- runCore $ ctProg (tcprog, env)
                return (cp, env)
        Nothing -> processMsgs msgs

loadGoal inp env = do
    parsed_res  <- liftIO $ runParser $ withInput inp $ parseGoal
    parsed_goal <- case parsed_res of
                        Right (g,_) -> return g
                        Left msgs   -> processMsgs msgs
    (tcres, msgs) <- liftIO $ runTc $ withSig parsed_goal $ withTypeEnv env $ wfg parsed_goal
    case tcres of
        Just (tcgoal, env') -> do
            cg <- runCore $ ctGoal (tcgoal, env')
            return cg
        Nothing -> processMsgs msgs
