module Backend where

import Pretty                               (pprint)
import qualified Infer                      (infer, prove)
import           ComputedAnswer             (ComputedAnswer)
import           Subst                      (Subst)
import           HopesIO                    (HopesContext(..), HopesState(..), gets)
import           Trace.Coroutine            (runTraceT, TraceT)
import           Core                       (CExpr)

import           Pipes.Core

import           Control.Monad.Trans        (liftIO, lift, MonadIO)



c =[ ("consult", 1)   -- interfers with the loadModule
   , ("type", 1)      -- interfers with the type environment
   , ("is", 2)        -- arithmetics
   , ("op", 3)        -- interfers with the operator table
   , ("listing", 1)
  ]


--infer :: (Monad m,
--          MonadState HopesState m) => CExpr -> Proxy x' x Bool ComputedAnswer m ()
infer e = do
    src <- lift $ gets assertions
    go src (Infer.prove e)
  where -- aux :: (Monad m, MonadIO m) => KnowledgeBase -> Infer.InferT (TraceT (CExpr,Subst) m) ComputedAnswer -> m (Maybe (ComputedAnswer, Infer.InferT (TraceT (CExpr,Subst) m) ComputedAnswer))
        aux src i =  traceT (Infer.infer src i)
        go src i = do
            result <- lift $ aux src i
            case result of
              Nothing -> return False
              Just (a, cont) -> do
                continue <- respond a
                if (continue)
                then go src cont
                else return True

traceT :: (Monad m, MonadIO m) => TraceT (CExpr,Subst) m b -> m b
traceT m = runTraceT m h
  where h (e,s) cont = do
          liftIO $ pprint e
          cont
