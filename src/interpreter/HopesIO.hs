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
  Command(..),
  HopesIO(..)
) where

import Operator (OperatorTable)
import Core     (KnowledgeBase, CPredDef, CExpr(..), ConstSym, PredSym)
import TypeCheck (PolySig, PredSig) -- change to Types(PolySig, PredSig)
import Subst

import Control.Applicative
import Control.Monad     (when)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Monoid      (mappend, mempty)
import Data.Map
import System.Directory (getCurrentDirectory)
import System.FilePath  (takeDirectory)

import Pipes (ListT(..), enumerate, next)

type BuiltinTable = Map PredSym (Builtin Subst)

data HopesState = HopesState
  { operators  :: OperatorTable
  , types      :: [PolySig PredSig]
  , assertions :: KnowledgeBase
  }

data HopesContext = HopesContext
  { workingDirectory :: FilePath
  , moduleName :: String
  , depth :: Int
  , builtins   :: BuiltinTable
  }

data Command =
    Assert  [CPredDef] [PolySig PredSig]
  | Query   CExpr
  | Command CExpr

newtype HopesIO a = HopesIO { unHopes:: ReaderT (HopesContext) (StateT (HopesState) IO) a }

instance Functor HopesIO where
  fmap f m = HopesIO (fmap f (unHopes m))

instance Applicative HopesIO where
  pure = HopesIO . pure
  x <*> y = HopesIO $ (unHopes x) <*> (unHopes y)

instance Monad HopesIO where
  return = HopesIO . return
  f >>= g = HopesIO $ (unHopes f) >>= \a -> unHopes (g a)

instance MonadIO HopesIO where
  liftIO = HopesIO . liftIO

class MonadHopes m where
  liftHopes :: HopesIO a -> m a

instance MonadHopes (HopesIO) where
  liftHopes = id

instance MonadReader HopesContext HopesIO where
  ask = HopesIO ask
  local f m = HopesIO $ local f (unHopes m)

instance MonadState HopesState HopesIO where
  state = HopesIO . state

instance (MonadTrans t, Monad m, MonadHopes m) => MonadHopes (t m) where
  liftHopes m = lift (liftHopes m)

runHopes h = do
      dir <- getCurrentDirectory
      let env = HopesContext dir defaultModule 0 mempty
      evalStateT (runReaderT (unHopes h) env) st
    where st = (HopesState { operators= mempty, types=mempty, assertions=mempty})
          defaultModule = "_top"

withFilename filename m =
  let dir = takeDirectory filename
  in local (\ctx -> ctx{workingDirectory = dir, depth = depth ctx + 1}) m

withBuiltins lst m =
  local (\ctx -> ctx{builtins = fromList lst}) m

lookupBuiltin sym = do
  tbl <- asks builtins
  return $ Data.Map.lookup sym tbl

logMsg msg = do
  d <- liftHopes $  asks depth
  liftIO $ putStr "%"
  liftIO $ putStr $ concat $ take d $ repeat "  "
  liftIO $ putStrLn $ msg

-- instance (MonadHopes m, Monad m) => MonadHopes (ReaderT r m) where
--  liftHopes m = lift (liftHopes m)

type Builtin = ListT (ReaderT [CExpr] (HopesIO))
-- type Builtin = ReaderT [CExpr] HopesIO

runBuiltin :: Builtin a -> [CExpr] -> HopesIO (Maybe (a, Builtin a))
runBuiltin m args = do
    r <- runReaderT (next (enumerate m)) args
    return $ fmap (\(a,p) -> (a, Select p)) (toMaybe r)
  where toMaybe (Left _) = Nothing
        toMaybe (Right a) = Just a

getArg :: Int -> Builtin CExpr
getArg i = do
  args <- ask
  when (i >= (length args)) $
    fail $ "Not enough arguments. Requested argument " ++ (show i)
  return (args !! i)

getArgAsSymbol i = do
  a <- getArg i
  case a of
     CConst c -> return c
     _ -> fail "Not of proper type"

getArgAsInt i = do
  a <- getArg i
  case a of
      CNumber (Left c) -> return $ fromIntegral c
      _ -> fail "Not of proper type"

getArgAsString = getArgAsSymbol
