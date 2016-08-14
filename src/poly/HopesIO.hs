module HopesIO (
  module HopesIO,
  gets, asks, modify,
  Command(..)
) where

import Operator (OperatorTable)
import Core     (KnowledgeBase, CPredDef, CExpr)
import TypeCheck (PolySig, PredSig) -- change to Types(PolySig, PredSig)


import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Monoid      (mappend, mempty)
import System.Directory (getCurrentDirectory)
import System.FilePath  (takeDirectory)


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
