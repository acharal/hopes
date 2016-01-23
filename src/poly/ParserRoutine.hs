module ParserRoutine where

import Syntax
import Loc
import Parser

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors


import Prelude hiding (getContents, readFile)
import Data.ByteString (ByteString)
import qualified Prelude (getContents, putStrLn, readFile)
import qualified Data.ByteString as ByteString (getContents, putStrLn, readFile)


parseAndYield :: Stream s m Char => Coroutine (Yield (SSent LocSpan)) (ParsecT s (ParseState s m) m) ()
parseAndYield = do
    r <- lift (try maybeEof <|> maybeSentence);
    case r of
      Just r' -> yield r'
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

parse2 p st inputFile = do
  input <- liftIO $ ByteString.readFile inputFile
  runPT p st inputFile input



runHopesParser3 st inputFile  = do
  parse2 (pogoStick f parseAndYield) st inputFile
  where f (Yield s c) = do
            when (isCommand s) (lift $ opDirective1 s)
            liftIO $ putStrLn "w"
            c
