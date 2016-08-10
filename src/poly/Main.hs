module Main where

import System.Environment (getArgs)

import Control.Monad.IO.Class
import Control.Monad.Trans

import Parser
-- import ParserRoutine
import Syntax
import Loc
import Desugar
import Prepr
import TypeCheck
import TcUtils
import Pretty (ppr, pprint, render)


import Prelude hiding (getContents, readFile)
import Data.ByteString (ByteString)
import qualified Prelude (getContents, putStrLn, readFile)
import qualified Data.ByteString as ByteString (getContents, putStrLn, readFile)

import Control.Monad.Error.Class


import Pipes
import qualified Pipes.Prelude as P

import Parser
import qualified Lexer as L

main = do
  args <- getArgs
  case args of
    (f:_) -> loadModuleFromFile f >> return ()
    _ -> return ()


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

loadModuleFromFile filename = do
      input <- liftIO $ ByteString.readFile filename
      liftIO $ runPipeline input pipeline
    where
      parse = parseSentences filename >-> P.map fixSentence
        where parseSentences :: (Monad m, Stream s m Char) => FilePath -> Producer (SSent LocSpan) (ParserT s m) ()
              parseSentences filename = parseAndYield

      typeCheck = P.map progToGroupDag >-> P.mapM (runTc initTcEnv)
      pipeline = groupUntil isCommand parse >-> typeCheck >-> P.map desugarGroup >-> P.show >-> P.stdoutLn

      runPipeline input p = runPT (runEffect p) parseState filename input
        where parseState = ParseSt [] [] [] []

      parseAndYield :: Stream s m Char => Producer (SSent LocSpan) (ParsecT s (ParseState s m) m) ()
      parseAndYield = do
          r <- lift (L.whiteSpace L.hopes >> (try maybeEof <|> maybeSentence));
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
{-

repl :: MonadIO m => Producing String String m ()
repl = loop
  where loop = do
          line   <- getInputLine "? "
          result <- request line
          liftIO $ putStrLn result
          loop

getInputLine prompt = liftIO $ do
  putStr prompt
  getLine

goalParser :: Monad m => Proxy () m String String Int String
goalParser = foreverK (\s -> lift (request (read s :: Int)) >>= request)

driver :: Monad m => Consuming () m Int String
driver = foreverK (\s -> request (show (s+1)))


collect :: Monad m => Producing a () m () -> m [a]
collect p = collect' [] p
  where collect' lst p  =  resume p >>= either (f lst) (\_ -> return lst)
        f l (Request x g) = collect' (x:l) (g ())


collect1 p h m1 m2 = collect' [] p
  where collect' lst p  =  resume p >>= either (f lst) (\_ -> m2 lst)
        f l (Request x g) = if (h x)
                            then m1 x l
                            else collect' [] (g ())
-}
{-
collectProxy x collected = do
  case x of
    SSent_comm command -> do
      respond (Right collected)
      respond (Left command)
      x' <- request ()
      collectProxy x' []
    SSent_clause clause -> do
      x' <- request ()
      collectProxy x' (clause:collected)
-}
{-
type Sentence = SSent LocSpan
type DesugaredSentence = SSent LocSpan
type Module = [DesugaredSentence]
type TyEnv = [(String,String)]
type TypedModule = (TyEnv, Module)

convertError :: MonadError e m =>  m (Either e a) -> m a
convertError m = m >>= either throwError return

loadModuleFromFile filename = do
  let parseState = ParseSt [] [] [] []
  input <- liftIO $ ByteString.readFile filename
  prog  <- convertError $ runPT (parseSentences filename $= preProcessSentences |> collect) parseState filename input
  liftIO $ mapM_ pprint prog

  -- parseSentences filename $= desugarer $$ executeOrReturn
  -- typeCheck $$ assert
  where -- produces sentences and requests nothing back
        parseSentences :: (Monad m, Stream s m Char) => FilePath -> Producing (Sentence) () (ParserT s m) ()
        parseSentences filename = mapSuspension (\(Yield i c) -> Request i (\()-> c)) parseAndYield

        -- consumes sentences and produces fixed sentences
        -- preProcessSentences :: Monad m => Proxy () m Sentence a Sentence a
        preProcessSentences = pipe fixSentence -- foreverK (\s -> respond (fixSentence s) >>= request)

        -- not working
        parseMultiple :: (Monad m, Stream s m Char) => Proxy () (ParserT s m) String () Sentence ()
        parseMultiple = foreverK (\f -> lift (parseSentences f) >>= request)

        -- parseFile filename = convertError (runPT  parseState filename input)

        -- desugar every parsed sentence
        x |> f = f x



        {-
        -- execute a directive and return a set of clauses at the end
        executeOrReturn :: Consuming Module m DesugaredSentence ()
        -- typecheck a module (banch of clauses) and produce a type environment
        typeCheckModule :: Module -> TypedModule

        assert :: TypedModule -> m ()
        -}
-}
{-
goalDriver :: Consuming () m Goal ComputedAnswer
goalDriver = parse $= desugarer $= typeCheckGoal $$ execute

executeOrReturn :: Consuming Module m DesugaredSentece ()
executeOrReturn sentence
  | isCommand sentence = execute sentence
  | otherwise = storeforlater

executeCommand :: Consuming () m Expr ComputedAnswer
executeCommand expr | isConsult expr  = loadModuleFromFile arg >> return
                    | isOperator expr = modifyOperator expr
                    | isType expr     =
                    | isCompilerFlag aa =
                    | isListing =
-}

{- Builtin functions
[ ("consult", 1)
, ("type", 1)
, ("is", 2)
, ("op", 3)
, ("listing", 1)
]
-}

{-
data HopesIO {
  operators :: OperatorTable,
  modules :: [HopesModule]
}

data HopesModule {
  name    :: String,
  fileName :: FilePath,
  clauses :: [Clauses],
  typeEnv :: TyEnv,
  exports :: [Predicates],
  operators :: OperatorTable
}

loadModule :: FilePath -> HopesIO ()


loadModule :: FilePath -> HopesIO ()
loadModule inputFile =
    parseFile inputFile (\sentence ->
      compile sentence;
      assert sentence;
      if (directive) executeDirective(sentence))

executeDirective(Consult file) = loadModule file
executeDirective(Op x y z) = addOperator x y z
-}

-- yield one or more answers. What about when no answers or exactly one answer produced?
-- execute :: Coroutine (Yield ComputerAnswer) m ()
-- debug :: Coroutine (Yield Breakpoint) m ()
-- parse :: Coroutine (Yield Sentence) m ()
