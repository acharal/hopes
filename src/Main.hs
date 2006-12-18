module Main where

import Parser
import ParseUtils
import Lexer
import IO
import System
import ErrUtils
import HOPL

import Refute
import LogicT
import Control.Monad.State
import Control.Monad.Identity


-- main = Parser.main

main = do
    (a1:a2:args) <- getArgs
    fileH <- openFile a1 ReadMode
    hoprog <- parseFromIO parseProg fileH
    hogoal <- case runP parseGoal (mkState a2) of
                PFailed _ err -> fail err
                POk _ g -> return g

    let p = transHOPLP hoprog
        g = map transHOPLA hogoal
{-
    print p
    print g
-}
    case runIdentity $ (runL Nothing (evalStateT (prove p g) 0)) of
    --case runIdentity $ evalStateT (runL Nothing (prove p g)) 0 of
        [] -> putStrLn "No"
        s  -> printSols s

    return ()


printSols [] = return ()
printSols (s:sl) = do
    printSol s
    c <- getChar
    if c /= 'q' then printSols sl else return ()

printSol [] = putStrLn "Yes"
printSol ((v,t):s) = 
    putStrLn (v ++ " = " ++ (show t)) >>
    printSol s

-- parseProg' :: IO Prog
parseFromIO f handle = do
    content <- hGetContents handle
    case runP f (mkState content) of
        PFailed _ err -> fail err
        POk _ x -> return x

transHOPLT :: HoTerm -> Term
transHOPLT (HoVar v)    = Var v
transHOPLT (HoConst c)  = Con c
transHOPLT (HoFun f tl) = Fun f (map transHOPLT tl)

transHOPLA :: HoAtom -> Atom
transHOPLA (HoAtom v tl) = Atom (Pre v) (map transHOPLT tl)

transHOPLP :: HoProg -> Prog
transHOPLP [] = []
transHOPLP ((h,b):rest) = (transHOPLA h, (map transHOPLA b)):(transHOPLP rest)

