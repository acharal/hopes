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

module HopesIO (
  module HopesIO,
  gets, asks, modify,
  Command(..)
) where

import Operator (OperatorTable)
import Core     (KnowledgeBase, CPredDef, CExpr(..), ConstSym)
import TypeCheck (PolySig, PredSig) -- change to Types(PolySig, PredSig)

import Control.Monad     (when)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Monoid      (mappend, mempty)
import System.Directory (getCurrentDirectory)
import System.FilePath  (takeDirectory)

import Pipes (ListT)

data HopesState = HopesState
  { operators  :: OperatorTable
  , types      :: [PolySig PredSig]
  , assertions :: KnowledgeBase
  }

data HopesContext = HopesContext
  { workingDirectory :: FilePath
  , moduleName :: String
  , depth :: Int
  }

data Command =
    Assert  [CPredDef] [PolySig PredSig]
  | Query   CExpr
  | Command CExpr

type HopesIO = ReaderT (HopesContext) (StateT (HopesState) IO)


runHopes h = do
      dir <- getCurrentDirectory
      let env = HopesContext dir defaultModule 0
      evalStateT (runReaderT h env) st
    where st = (HopesState { operators= mempty, types=mempty, assertions=mempty})
          defaultModule = "_top"

withFilename filename m =
  let dir = takeDirectory filename
  in local (\ctx -> ctx{workingDirectory = dir, depth = depth ctx + 1}) m

logMsg msg = do
  d <- asks depth
  liftIO $ putStr "%"
  liftIO $ putStr $ concat $ take d $ repeat "  "
  liftIO $ putStrLn $ msg


--type Builtin = ReaderT [CExpr] (ListT HopesIO)
type Builtin = ReaderT [CExpr] HopesIO

runBuiltin m args = runReaderT m args

liftHopesIO :: HopesIO a -> Builtin a
liftHopesIO = lift

getArgAsSymbol :: Int -> Builtin ConstSym
getArgAsSymbol i = do
  args <- ask
  when (i >= (length args)) $
    fail $ "Not enough arguments. Requested argument " ++ (show i)

  case args !! i of
     CConst c -> return c
     _ -> fail "Not of proper type"


getArgAsInt :: Int -> Builtin Int
getArgAsInt i = do
  args <- ask
  when (i >= (length args)) $
    fail $ "Not enough arguments. Requested argument " ++ (show i)

  case args !! i of
      CNumber (Left c) -> return $ fromIntegral c
      _ -> fail "Not of proper type"

getArgAsString = getArgAsSymbol
