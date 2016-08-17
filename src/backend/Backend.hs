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

module Backend where

import Pretty                               (pprint)
import qualified Infer                      (infer, prove)
import           ComputedAnswer             (ComputedAnswer)
import           Subst                      (Subst)
import           HopesIO                    (HopesContext(..), HopesState(..), gets)
import           Trace.Coroutine            (runTraceT, TraceT)
import           Core                       (CExpr)

import           Pipes.Core
import           Control.Monad              (when)
import           Control.Monad.Trans        (liftIO, lift, MonadIO)


--infer :: (Monad m,
--          MonadState HopesState m) => CExpr -> Proxy x' x Bool ComputedAnswer m ()
infer e = do
    src <- lift $ gets assertions
    go src (Infer.prove e) False
  where -- aux :: (Monad m, MonadIO m) => KnowledgeBase -> Infer.InferT (TraceT (CExpr,Subst) m) ComputedAnswer -> m (Maybe (ComputedAnswer, Infer.InferT (TraceT (CExpr,Subst) m) ComputedAnswer))
        aux src i =  traceT (Infer.infer src i)
        go src i b = do
            result <- lift $ aux src i
            case result of
              Nothing -> return b
              Just (a, cont) -> do
                continue <- respond a
                case continue of
                    True -> go src cont True
                    False -> return True

traceT :: (Monad m, MonadIO m) => TraceT (CExpr,Subst) m b -> m b
traceT m = runTraceT m (\_ -> id)
