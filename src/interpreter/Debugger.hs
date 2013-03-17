
module Debugger (attachDebugger) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class ()
import Control.Monad.State.Class

import Trace
import Pretty
import HopesIO
import System.IO

-- Debugger Options
-- 
-- 'h'      help for debugger
-- 'a'      abort evaluation
-- 'e'      exit - terminate interpreter
-- 
-- 'f'      fail current branch
-- 'r'      retry
-- 'newline' continue to next step and wait
--          step without stop
--          step until success (answer) and wait
--          step until fail or answer and wait
-- 
-- 'n' no debug - turn debug off (do not wait or print)
--
-- 'v' view answer so far
-- 'p' print current goal

-- Other Features in the Wish-List
-- 1. set maximum depth of derivations
-- 2. number derivation and match "call" and "fail" with the same number
-- 3. print if more branches where found after a derivation (and if possible how many)

attachDebugger m = debug m debug_handler

data DebugState i m a = 
    DSt { count :: Int,
          retry_cont :: DebugT (DebugState i m a) i m a,
          prev_cont :: DebugT (DebugState i m a) i m a
        }

data DebugRequest a = Debug Char

data DebugCommand = 
      DebugAbort
    | DebugFailBranch
    | DebugRetry
    | DebugNext
    | DebugOff
    | DebugHelp


commands = [
      ('a', DebugAbort)    
    , ('f', DebugFailBranch)
    , ('r', DebugRetry)
    , ('\n', DebugNext)
    , ('n', DebugOff)
    , ('h', DebugHelp)
    ]

lookupCommand c = lookup c commands

data DebugOptions = DebugOptions 
    { stopInEachStep :: Bool -- True will wait for "DebugNext" to continue
    , printEachTrace :: Bool -- False is silent
    , maxDepth :: Maybe Int  -- Just i is the maximum depth of steps (derivations)
    }

debug m h = runDebugT st m h
    where st = DSt { count = 1
                   , retry_cont = fail ""
                   , prev_cont  = fail "" 
                   }


debug_handler (g, s) cont = 
    let prompt = do
            liftIO $ putStr " ? "
            c <- liftIO $ getChar 
            when (c /= '\n') $
                liftIO (putChar '\n')
            return $ lookupCommand c

        handleOptions cont = do
            opt <- prompt
            
            case opt of 
                Nothing -> handleOptions cont
                Just command ->
                    handle cont command
    in do
        liftIO $ hSetBuffering stdout NoBuffering
        flag <- gets debugFlag
        c <- getsState count
        modifyState (\s -> s{count = c + 1})

        if (flag) 
         then do
            liftIO $ putStr $ show $ nest 5 $ parens (int c) $$ nest 5 (ppr g)
            handleOptions cont
         else 
            cont   

goto cont = do
    prev <- getsState prev_cont
    modifyState (\s -> s{ prev_cont = (goto cont), retry_cont = prev })
    cont


handle cont DebugAbort = fail "failed by user"
handle cont DebugFailBranch = do
    lift $ fail "fail branch by user"
    goto cont

handle cont DebugNext = goto cont
handle cont DebugRetry = do
    retry <- getsState retry_cont
    retry

handle cont DebugOff = do
    modify (\s -> s{debugFlag = False})
    goto cont

nodebug _ cont = cont

