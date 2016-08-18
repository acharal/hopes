--  Copyright (C) 2016 Angelos Charalambidis  <a.charalambidis@di.uoa.gr>
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

import Frontend                             (processQuery, processFile)
import Backend                              (infer)
import HopesIO                              (withFilename,asks,logMsg,withBuiltins,MonadHopes,HopesIO(..), runHopes, HopesContext(..), HopesState(..), Command(..), HopesIO, modify, gets, getArgAsInt, getArgAsString, liftHopes, runBuiltin)
import Core                                 (CExpr(..), fromList)
import Error                                (runExceptT)
import Pretty                               (pprint)

import           Pipes.Core

import           Data.Monoid                (mappend, mempty)
import           Control.Monad.Trans        (MonadIO, lift, liftIO)
import           Control.Monad.State.Class  (MonadState(..))
import           System.Console.Haskeline   (runInputT, InputT, getInputLine, defaultSettings)
import           System.Console.Haskeline.MonadException
import           System.IO                  (hSetBuffering, stdin, BufferMode(NoBuffering))
import           System.Environment         (getArgs)

-- temporary imports (to be removed)
import           Operator                   (Operator(..))

import           System.FilePath  ((</>), normalise)
import           System.Directory (canonicalizePath)

import Builtin

main = do
  args <- getArgs
  runHopes $ withBuiltins builtin $ do
    mapM_ includeFile args
    prog <- gets assertions
    runInputT defaultSettings $ runEffect (readLine +>> repl)
    return ()

includeFile file = do
  dir <- asks workingDirectory
  absoluteFilename <- liftIO $ canonicalizePath (normalise $ dir </> file)
  logMsg $ "Loading " ++ absoluteFilename
  withFilename absoluteFilename $ runEffect (executeCommand >\\ (processFile absoluteFilename))
  logMsg $ "Loaded " ++ absoluteFilename

instance MonadState s m => MonadState s (InputT m) where
  state = lift . state

instance MonadException HopesIO where
  controlIO f = HopesIO $ controlIO $ \(RunIO run) -> let
                        run' = RunIO (fmap HopesIO . run . unHopes)
                        in fmap unHopes $ f run'

readLine prompt = do
  line <- lift $ getInputLine prompt
  case line of
    Just l -> do
      p <- respond l
      readLine p
    Nothing ->
      return ()

--repl :: (Monad m, MonadIO m, MonadState HopesState m, MonadHopes m) => Proxy String String y0 x0 m ()
repl = loop
  where loop = do
          line <- request "?- "
          e <- lift $ runExceptT $ runEffect $ (queryDriver />/ printResultAndWait) (line :: String)
          case e of
            Left (msgs,errors) -> do
              liftIO $ pprint msgs
              liftIO $ pprint errors
            Right hasResults -> do
              liftIO $ putStrLn (yesno hasResults)
          loop
        yesno True = "Yes"
        yesno False = "No"

-- printResultAndWait :: (Pretty a, MonadIO m) => a -> m Bool
printResultAndWait result = do
  liftIO $ pprint result
  liftIO $ hSetBuffering stdin NoBuffering
  c <- liftIO $ getChar
  return (c == ';')

continueQuietly result = return True

-- goalDriver :: (Monad m, MonadError Messages m) =>  String -> Proxy X () Bool ComputedAnswer m ()
queryDriver queryString = do
  g <- lift $ processQuery queryString
  infer g

-- must be moved probably in Backend
--executeCommand :: Command -> HopesIO ()
executeCommand (Assert prog tyEnv)  =
  modify (\e -> e{ assertions = (assertions e) `mappend` (fromList prog)
                 , types = (types e) `mappend` tyEnv
                 })
executeCommand (Command comm) = do
  b <- runEffect $ (infer />/ continueQuietly) comm
  case b of
    True  -> return ()
    False -> fail "command failed"

executeCommand (Query query)  = do
  b <- runEffect $ (infer />/ continueQuietly) query
  case b of
    True  -> return ()
    False -> fail "query failed"

builtin = [
    (("op", 3),      commandOp)
  , (("include", 1), commandInclude)
  , (("is",2),       is)
  ]

commandInclude = do
  file <- getArgAsString 0
  liftHopes $ includeFile file
  return []

commandOp = do
  prec   <- getArgAsInt 0
  assoc  <- getArgAsString 1
  opname <- getArgAsString 2
  let op = Operator { opName = opname, opAssoc = assoc }
  liftHopes $ modify (\e -> e{operators = (fromIntegral prec :: Int, op):(operators e)})
  return []
