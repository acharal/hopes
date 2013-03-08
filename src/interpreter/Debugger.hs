
module Debugger (module Debugger, debug) where

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

debug_handler (g, s) cont = 
    let handleOptions cont = do
            opt <- liftIO getChar
            case opt of
                'h' -> liftIO $ print "I need help too!"
                'a' -> fail "failed by user"
                'f' -> lift (fail "fail branch by user")
                'n' -> modify (\s -> s{debugFlag = False})
                _ -> return ()
    in do
        flag <- gets debugFlag

        when (flag) $ do
            liftIO $ putStrLn $ show $ ppr g
            handleOptions cont

        cont
    

nodebug _ cont = cont

