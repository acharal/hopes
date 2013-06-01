{-
 - A main module useful for testing
 -}


module Main where

import Basic
import Error
import Pretty
import Syntax
import Parser
import Prepr
import TcUtils



test file testThis = do 
    let fileFull = "../../pl/examples/" ++ file ++".pl" 
    (a,b) <- parseProlog2 fileFull
    testThis a
 
testParse a = mapM_ (\x-> putStrLn $ show x) a
testPre   a = mapM_ (\x-> putStrLn $ show x) (progToGroupDag a)

simple = "simple"
aleph  = "aleph"
