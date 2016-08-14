module Main where

import Frontend                             (processQuery, processFile)
import HopesIO                              (runHopes, HopesContext(..), HopesState(..))
import Core                                 (CExpr)
import Error                                (runExceptT)
import Pretty                               (pprint)

import qualified Infer                      (infer, prove)
import           ComputedAnswer             (ComputedAnswer)
import           Subst                      (Subst)
import           Trace.Coroutine            (runTraceT, TraceT)

import           Pipes.Core

import           Control.Monad.Trans        (MonadIO, lift, liftIO)
import           Control.Monad.State.Class  (MonadState(..), gets)
import           System.Console.Haskeline   (runInputT, InputT, getInputLine, defaultSettings)
import           System.IO                  (hSetBuffering, stdin, BufferMode(NoBuffering))
import           System.Environment         (getArgs)

c =[ ("consult", 1)   -- interfers with the loadModule
   , ("type", 1)      -- interfers with the type environment
   , ("is", 2)        -- arithmetics
   , ("op", 3)        -- interfers with the operator table
   , ("listing", 1)
  ]

main = do
  args <- getArgs
  runHopes $ do
    mapM_ processFile args
    prog <- gets assertions
    runInputT defaultSettings $ runEffect (readLine +>> repl)
    return ()

instance MonadState s m => MonadState s (InputT m) where
  state = lift . state

readLine prompt = do
  line <- lift $ getInputLine prompt
  case line of
    Just l -> do
      p <- respond l
      readLine p
    Nothing ->
      return ()

repl :: (Monad m, MonadIO m, MonadState HopesState m) => Proxy String String y0 x0 m ()
repl = loop
  where loop = do
          line <- request "?- "
          e <- lift $ runExceptT $ runEffect $ (queryDriver />/ printResultAndWait) (line :: String)
          case e of
            Left (msgs,errors) -> do
              liftIO $ pprint msgs
              liftIO $ pprint errors
            Right hasResults -> do
              if (hasResults)
              then liftIO $ putStrLn "Yes"
              else liftIO $ putStrLn "No"
          loop


-- printResultAndWait :: (Pretty a, MonadIO m) => a -> m Bool
printResultAndWait result = do
  liftIO $ pprint result
  liftIO $ hSetBuffering stdin NoBuffering
  c <- liftIO $ getChar
  return (c == ';')

-- goalDriver :: (Monad m, MonadError Messages m) =>  String -> Proxy X () Bool ComputedAnswer m ()
queryDriver queryString = do
  g <- lift $ processQuery queryString
  infer g

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
