module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Error.Class
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.Monoid               (mappend, mempty)

-- import ParserRoutine
import Syntax
import Core
import Loc
import Desugar
import Prepr
import TypeCheck
import Error
import TcUtils (initTcEnv, withEnvPreds, Tc, PredSig, PolySig, runTcT)
import Types
import Pretty (Pretty, text, (<+>), render, ppr, pprint)
import Operator

import           Parser                    hiding (operators, parse)
import qualified Parser                    (operators)
import           Prelude                   hiding (getContents, readFile)
import qualified Prelude                   (getContents, readFile, putStrLn, putStr)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as ByteString (getContents, putStrLn, readFile)



import           Pipes
import           Pipes.Core
import qualified Pipes.Prelude             as P

import qualified Lexer                     as L

import           System.Directory          (canonicalizePath, getCurrentDirectory)
import           System.FilePath
import           System.Environment        (getArgs)
import           System.IO

import qualified Infer
import           ComputedAnswer
import           Subst
import           Trace.Coroutine
import           System.Console.Haskeline
import           System.Console.Haskeline.MonadException ()

instance MonadState s m => MonadState s (InputT m) where
  state = lift . state

main = do
  args <- getArgs
  runHopes $ do
    mapM_ loadFile args
    prog <- gets assertions
    runInputT defaultSettings $ runEffect (readLine +>> repl)
    return ()

readLine prompt = do
  line <- lift $ getInputLine prompt
  case line of
    Just l -> do
      p <- respond l
      readLine p
    Nothing ->
      return ()

--groupUntil :: Monad m => (a -> Bool) -> Producer' a m r -> Producer' [a] m r
groupUntil predicate p = go id p
  where
    go diffAs p = do
      x <- lift (next p)
      case x of
        Left r ->
          case (diffAs []) of
            []    -> return r
            (x:xs) -> do
              yield (x:xs)
              return r
        Right (a, p') ->
            if (predicate a)
              then do
                yield ((diffAs . (a:)) [])
                go id p'
              else go (diffAs . (a:)) p'

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

type HopesIO = ReaderT (HopesContext) (StateT (HopesState) IO)

runHopes h = do
      dir <- getCurrentDirectory
      let env = HopesContext dir defaultModule 0
      evalStateT (runReaderT h env) st
    where st = (HopesState { operators= mempty, types=mempty, assertions=mempty})
          defaultModule = "_top"

data Command =
    Assert  [CPredDef] [PolySig PredSig]
  | Query   CExpr
  | Command CExpr

c =[ ("consult", 1)   -- interfers with the loadModule
   , ("type", 1)      -- interfers with the type environment
   , ("is", 2)        -- arithmetics
   , ("op", 3)        -- interfers with the operator table
   , ("listing", 1)
  ]

executeCommand :: Command -> HopesIO ()
executeCommand (Assert prog tyEnv)  =
  modify (\e -> e{ assertions = (assertions e) `mappend` (fromList prog)
                 , types = (types e) `mappend` tyEnv
                 })
executeCommand (Command comm) =
  case c comm of
    Just (("op", 3), [CNumber (Left prec),CConst assoc, CConst opname]) -> do
      let op = Operator { opName = opname, opAssoc = assoc }
      modify (\e -> e{operators = (fromIntegral prec :: Int, op):(operators e)})
    Just (("$include", 1), [CConst file]) -> do
      loadFile file
      return ()
    _ -> return ()
  where c (CApp _ (CPred _ p) args) = Just (p, args)
        c _ = Nothing
executeCommand (Query query)  = return ()

type Loader m = Tc LocSpan (ParsecT ByteString (ParseState ByteString m) m)
type Sentence = SSent LocSpan
type SourceProgram = SProg (Typed LocSpan)
type PredDefs = [CPredDef]
type Goals = [CExpr]
type Commands = [CExpr]

withFilename filename m =
  let dir = takeDirectory filename
  in withReaderT (\ctx -> ctx{workingDirectory = dir, depth = depth ctx + 1}) m

logMsg msg = do
  d <- asks depth
  liftIO $ Prelude.putStr "%"
  liftIO $ Prelude.putStr $ concat $ take d $ repeat "  "
  liftIO $ Prelude.putStrLn $ msg

loadFile :: FilePath -> HopesIO (Either Messages ())
loadFile filename = do
  dir <- asks workingDirectory
  absoluteFilename <- liftIO $ canonicalizePath (normalise $ dir </> filename)
  withFilename absoluteFilename $ do
      logMsg $ "Loading " ++ absoluteFilename
      input <- liftIO $ ByteString.readFile absoluteFilename
      r <- runPipeline absoluteFilename input pipeline
      logMsg $ "Loaded " ++ absoluteFilename
      return r
--      return ()

--runPipeline :: SourceName -> ByteString -> Effect (Loader HopesIO) () -> HopesIO (Either Messages ())
runPipeline filename input p = do
            op <- gets operators
            r <- runPT (buildTable >> runTcT initTcEnv (runEffect p)) (parseState op) filename input
            return (join r)
        where parseState op = ParseSt op [] [] []

parse :: (Monad m, Stream s m Char) => Producer (SSent LocSpan) (Tc a (ParsecT s (ParseState s m) m)) ()
parse = parseAndYield >-> P.map fixSentence

typeCheck = P.map sentencesToProg >-> (tc [])

tc env = do
    dag <- await
    tcOut <- lift (withEnvPreds env $ tcProg dag)
    yield tcOut
    tc (tcOutPreds tcOut)

toCore = P.map (\tcout -> (desugarProg (tcOutSyntax tcout), tcOutPreds tcout))

toCommands = do
  ((prog, commands, goals), tyenv) <- await
  when (not (null prog)) (yield (Assert prog tyenv))
  when (not (null commands)) $
    each $ map (Command) commands
  when (not (null goals)) $
    each $ map (Query) goals
  toCommands

pipeline = groupUntil isCommand parse >->
            typeCheck >->
            toCore >->
            toCommands >->
            P.mapM_ execCommand
      where execCommand x = do
                lift $ lift $ executeCommand x
                ops <- lift $ lift $ (gets operators)
                lift $ updateState (\st -> st{ Parser.operators = ops})
                lift $ buildTable

parseAndYield :: (Monad m, Stream s m Char) => Producer (SSent LocSpan) (Tc a (ParsecT s (ParseState s m) m)) ()
parseAndYield = do
    r <- lift $ lift $ (L.whiteSpace L.hopes >> (try maybeEof <|> maybeSentence));
    case r of
      Just r' -> yield r' >> parseAndYield
      Nothing -> return ()
  where
        maybeSentence :: Stream s m Char => ParserT s m (Maybe (SSent LocSpan))
        maybeSentence = do
            s <- sentence
            return (Just s)
        maybeEof :: Stream s m Char => ParserT s m (Maybe (SSent LocSpan))
        maybeEof = do
            eof
            return Nothing

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


printResultAndWait :: (Pretty a, MonadIO m) => a -> m Bool
printResultAndWait result = do
  liftIO $ pprint result
  liftIO $ hSetBuffering stdin NoBuffering
  c <- liftIO $ getChar
  return (c == ';')

-- goalDriver :: (Monad m, MonadError Messages m) =>  String -> Proxy X () Bool ComputedAnswer m ()
queryDriver queryString = do
  g <- lift $ processQuery queryString
  infer g

--goalDriver :: String -> HopesIO ()
processQuery queryString = do
    e <- parse queryString
    e' <- typeCheck e
    return (desugarGoal e')
    where parse input = exceptT $ do
              op <- gets operators
              runPT gp (parseState op) "stdin" input
          typeCheck e = exceptT $ do
              let rho_o = Rho_pi Pi_o
              tyenv <- gets types
              tye <- runTcT initTcEnv $ withEnvPreds tyenv $ tcGoal e
              return tye
          parseState op = ParseSt op [] [] []
          gp = do   { pos1 <- getPosition
                    ; ex <- try fullExpr <|> (allExpr True)
                    ; pos2 <- getPosition
                    ; symbol "."
                    ; let pos2' = incSourceColumn pos2 1
                    ; return $ SGoal (mkSpan pos1 pos2')  (fixExpr True 0 ex)
                    }
          exceptT m = (lift m) >>= \r ->
            case r of
              Left e -> throwError e
              Right a -> return a


--infer :: (Monad m,
--          MonadState HopesState m) => CExpr -> Proxy x' x Bool ComputedAnswer m ()
infer e = do
    src <- lift $ gets assertions
    go src (Infer.prove e)
  where aux :: (Monad m, MonadIO m) => KnowledgeBase -> Infer.InferT (TraceT (CExpr,Subst) m) ComputedAnswer -> m (Maybe (ComputedAnswer, Infer.InferT (TraceT (CExpr,Subst) m) ComputedAnswer))
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
