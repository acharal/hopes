
module Debugger (attachDebugger) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class ()
import Control.Monad.State.Class

import Trace
import Pretty
import HopesIO

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

debug m h = runDebugT st m h
    where st = DSt { count = 0, retry_cont = fail "", prev_cont = fail "" }


debug_handler (g, s) cont = 
    let handleOptions cont = do
            opt <- liftIO getChar

            case opt of
                'h' -> (liftIO $ print "I need help too!") >> goto cont
                'a' -> fail "failed by user"
                'f' -> (lift (fail "fail branch by user")) >> goto cont
                'n' -> modify (\s -> s{debugFlag = False}) >> goto cont
                'r' -> getsState retry_cont >>= \c -> c
                _ -> goto cont
        goto cont = do
            prev <- getsState prev_cont
            modifyState (\s -> s{ prev_cont = (goto cont), retry_cont = prev })
            cont
    in do

        flag <- gets debugFlag
        c <- getsState count
        modifyState (\s -> s{count = c + 1})

        if (flag) 
         then do
            liftIO $ putStrLn $ show $ int c <+> ppr g
            handleOptions cont
         else 
            cont   

nodebug _ cont = cont

