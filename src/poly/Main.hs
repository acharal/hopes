module Main where

import System.Environment (getArgs)

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class
import Control.Monad.Trans

import Parser
import ParserRoutine
import Syntax
import Loc
import Desugar
import Prepr
import Pretty (ppr, pprint)


import Prelude hiding (getContents, readFile)
import Data.ByteString (ByteString)
import qualified Prelude (getContents, putStrLn, readFile)
import qualified Data.ByteString as ByteString (getContents, putStrLn, readFile)

main = do
  args <- getArgs
  shell $= goalParser $$ driver
  return ()


shell :: MonadIO m => Producing String String m ()
shell = loop
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

-- collectProxy :: Monad m => Proxy () m () (Either [SClause LocSpan] (SCommand LocSpan)) ()

type Sentence = SSent LocSpan
type DesugaredSentence = SSent LocSpan
type Module = [DesugaredSentence]
type TyEnv = [(String,String)]
type TypedModule = (TyEnv, Module)

loadModuleFromFile filename = do
  let parseState = ParseSt [] [] [] []
  input <- liftIO $ ByteString.readFile filename
  case runP (parseSentences filename $= preProcessSentences |> collect) parseState filename input of
    Left errs  -> liftIO $ putStrLn (show errs)
    Right prog -> liftIO $ mapM_ pprint prog

  -- parseSentences filename $= desugarer $$ executeOrReturn
  -- typeCheck $$ assert
  where -- produces sentences and requests nothing back
        parseSentences :: (Monad m, Stream s m Char) => FilePath -> Producing (Sentence) () (ParserT s m) ()
        parseSentences filename = mapSuspension (\(Yield i c) -> Request i (\()-> c)) parseAndYield

        -- consumes sentences and produces fixed sentences
        preProcessSentences :: Monad m => Proxy () m Sentence a Sentence a
        preProcessSentences = foreverK (\s -> lift (request (fixSentence s)) >>= request)

        -- not working
        parseMultiple :: (Monad m, Stream s m Char) => Proxy () (ParserT s m) String () Sentence ()
        parseMultiple = foreverK (\f -> lift (parseSentences f) >>= request)

        -- desugar every parsed sentence
        x |> f = f x
        {-
        -- execute a directive and return a set of clauses at the end
        executeOrReturn :: Consuming Module m DesugaredSentence ()
        -- typecheck a module (banch of clauses) and produce a type environment
        typeCheckModule :: Module -> TypedModule

        assert :: TypedModule -> m ()
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



{-
  Coroutine playground
  consult
  https://hackage.haskell.org/package/monad-coroutine-0.9.0.1
  https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/coroutines-for-streaming/part-3-stacking-interfaces
-}

type Producing o i = Coroutine (Request o i)
type Consuming r m i o = i -> Producing o i m r

($$) :: Monad m => Producing i o m r -> Consuming r m i o -> m r
producer $$ consumer = resume producer >>= either f return
  where f (Request x g) = consumer x $$ g

foreverK :: Monad m => (a -> m a) -> a -> m b
foreverK m a = m a >>= foreverK m


consume :: Monad m => (a -> b) -> Consuming r m a b
consume f = foreverK (\x -> request (f x))

fuse :: Monad m => Consuming r m a b -> Consuming r m b c -> Consuming r m a c
fuse c1 c2 = \a -> lift (resume (c1 a)) >>= either f return
  where f (Request b g) = lift (resume (c2 b)) >>= either (h g) return
        h g1 (Request c g2) = suspend $ Request c (fuse g1 g2)


echo :: Monad m => Consuming r m a a
echo = foreverK request

-- Proxies

type Proxy r m i1 o1 i2 o2 =  Consuming r (Producing i2 o2 m) i1 o1

idProxy :: Monad m => Proxy r m a b a b
idProxy = \a -> foreverK (\x -> do { y <- lift (request x); request y }) a


insert0 :: (Monad m, Functor s) => m a -> Coroutine s m a
insert0 = lift
insert1 :: (Monad m, Functor s, Functor s1) => Coroutine s1 m a -> Coroutine s1 (Coroutine s m) a
insert1 = mapMonad insert0
insert2 :: (Monad m, Functor s, Functor s0, Functor s1) => Coroutine s0 (Coroutine s1 m) a -> Coroutine s0 (Coroutine s1 (Coroutine s m)) a
insert2 = mapMonad insert1

commute ::  forall a b c d m r. Monad m => Producing a b (Producing c d m) r -> Producing c d (Producing a b m) r
commute p = p' $$ funnel
  where --p' :: Producing a b (Producing c d (Producing a b m)) r
        p' = insert2 p
        --funnel :: Consuming r (Producing c d (Producing a b m)) a b
        funnel a = insert1 (idProxy a)

($=) :: Monad m => Producing a b m r -> Proxy r m a b c d -> Producing c d m r
p $= proxy = insert1 p $$ proxy

(=$) :: Monad m => Proxy r m a b c d -> Consuming r m c d -> Consuming r m a b
proxy =$ c = \a -> p a $$ (insert1 . c)
  where p a = commute (proxy a)

(=$=) :: Monad m => Proxy r m a a' b b' -> Proxy r m b b' c c' -> Proxy r m a a' c c'
proxy1 =$= proxy2 = \a -> p a $$ c
  where p a = insert2 $ commute $ proxy1 a
        c a = insert1 $ proxy2 a
