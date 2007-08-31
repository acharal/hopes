module Main where

import IO
import System
import System.IO
import Parser
import Pretty
import Err

import Tc
import Wffc
import Syntax
--import Hopl
--import ProofProc
--import Logic

import Types

main = do
    (f:_) <- getArgs
    (res,msgs) <- loadSource f

    case res of
        Just (p, env) -> do
            pprint p
            --print $ ppr_env env
        Nothing ->  
            pprint $ vcat (map ppr (processMsgs msgs))


--loadSource :: String -> IO (Maybe (Prog, TypeEnv), Messages)
loadSource file = do
    parse_res <- parseFromFile parseSrc file
    case parse_res of
        Right (p, s) -> do
            (p', msgs) <- runTc (tcProg p >>= wffcProg)
            return (p', msgs)
        Left msgs -> 
            return (Nothing, msgs)

{-
loadSource :: String -> IO (Maybe (Prog, TypeEnv), Messages)
loadSource file = do
    parse_res <- parseFromFile parseSrc file
    case parse_res of
        Right (p, s) -> do
            case runTc (tcSource p) of
                (Just (p', env), msgs) -> do
                    let p'' = runSimple (simplifyProg p')
                    return (Just (p'', env), msgs)
                (Nothing, msgs) ->
                    return (Nothing, msgs)
        Left msgs -> 
            return (Nothing, msgs)


takeGoal :: (Prog, TypeEnv) -> IO Goal
takeGoal (p, env) = do
    inp <- hGetLine stdin
    case runParser (parseGoal) (mkState inp) of
        Right (g, s) -> do
            case runTcWithEnv env (tcGoal (postParseGoal env g)) of
                (Just g', msgs) -> do
                    let g'' = runSimple (simplifyGoal g')
                    return g''
                (Nothing, msgs) -> do
                    pprint (vcat (map ppr (processMsgs msgs)))
                    takeGoal (p, env)
        Left msgs -> do
            pprint $ vcat (map ppr (processMsgs msgs))
            takeGoal (p, env)

printSol [] = pprint (text "No")
printSol ([]:xs) = pprint (text "Yes")
printSol (x:xs) = do
    pprint x
    c <- getChar
    case c of
        'q' -> pprint (text "Yes")
        _ -> printSol xs

-}

ppr_env env = vcat (map ppr_aux env)
    where ppr_aux (v, t) = hang ((ppr v) <+> (text "::")) (length (show v) + 4) $ sep [(ppr t), text "order", int (order t)]
