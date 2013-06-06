{-
 - A main module for testing
 -}


module Main where

import Basic
import Error
import Pretty
import Syntax
import Parser
import Prepr
import TcUtils
import TypeCheck

import Control.Monad.Identity

test file testThis = do 
    let fileFull = "../../pl/examples/" ++ file ++".pl" 
    (a,b) <- parseProlog2 fileFull
    testThis a
 
testParse a = mapM_ (\x-> putStrLn $ show x) a
testPre   a = mapM_ (\x-> putStrLn $ show x) (progToGroupDag a)
testTc    a = case (runIdentity $ runTc emptyTcEnv $ progToGroupDag a) of
    Left msgs -> mapM_ (putStrLn . show . ppr) (fst msgs)
    Right dag -> mapM_ (putStrLn.show) (tcOutPreds dag)
                    
simple = "simple"
aleph  = "aleph"
