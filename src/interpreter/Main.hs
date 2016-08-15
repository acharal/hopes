module Main where

import Frontend                             (processQuery, processFile)
import Backend                              (infer)
import HopesIO                              (runHopes, HopesContext(..), HopesState(..), Command(..), HopesIO, modify, gets, getArgAsInt, getArgAsString, liftHopesIO, runBuiltin)
import Core                                 (CExpr(..), fromList)
import Error                                (runExceptT)
import Pretty                               (pprint)

import           Pipes.Core

import           Data.Monoid                (mappend, mempty)
import           Control.Monad.Trans        (MonadIO, lift, liftIO)
import           Control.Monad.State.Class  (MonadState(..))
import           System.Console.Haskeline   (runInputT, InputT, getInputLine, defaultSettings)
import           System.IO                  (hSetBuffering, stdin, BufferMode(NoBuffering))
import           System.Environment         (getArgs)

-- temporary imports (to be removed)
import           Operator                   (Operator(..))


main = do
  args <- getArgs
  runHopes $ do
    mapM_ includeFile args
    prog <- gets assertions
    runInputT defaultSettings $ runEffect (readLine +>> repl)
    return ()

includeFile file = runEffect (executeCommand >\\ (processFile file))

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
executeCommand (Command comm) =
  case splitCommand comm of
    Just (f, args) ->
      case lookup f builtin of
        Just fdef -> lift $ runBuiltin fdef args
        Nothing -> return ()
    _ -> return ()
  where splitCommand (CApp f (CPred _ p) args) = Just (p, args)
        splitCommand (CPred _ p) = Just (p, [])
        splitCommand _ = Nothing

executeCommand (Query query)  = return ()

builtin = [
    (("op", 3),      commandOp)
  , (("include", 1), commandInclude)
  ]

commandInclude = do
  file <- getArgAsString 0
  liftHopesIO $ do
    includeFile file
    return ()

commandOp = do
  prec   <- getArgAsInt 0
  assoc  <- getArgAsString 1
  opname <- getArgAsString 2
  let op = Operator { opName = opname, opAssoc = assoc }
  liftHopesIO $ modify (\e -> e{operators = (fromIntegral prec :: Int, op):(operators e)})
