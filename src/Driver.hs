--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

-- | drives the pipeline of compilation + execution
module Driver where

import Lang(liftSym)
import Syntax(HpSymbol)
import Parser(runParser, withInput, parseSrc, parseGoal, fromFile)
import Types(TyEnv, Typed, findTySig)
import Tc(runTc, withSig, withTypeEnv)
import WellForm(wfp, wfg)
import Desugar
import Hopl
import Infer
import Pretty
import System.IO
import System(exitWith, ExitCode(..))
--import Error(catchError, when)
import Control.Monad.State(StateT, modify, gets, evalStateT)
import Shell
-- import Data.Function( fix )
import Data.Monoid
import KnowledgeBase

import Control.Monad.Cont

data HopeEnv =
    HEnv {  
        currentEnv :: TyEnv HpSymbol,
        kb         :: KnowledgeBase (Typed HpSymbol)
    }

type HopesIO = StateT HopeEnv (ShellT IO)


buildin_preds :: [(String, Int, [String]-> HopesIO (), String -> IO [String])]
buildin_preds =
 [ ("listing", 1, undefined, undefined)
 , ("typeof",  1, undefined, undefined)
 , ("consult", 1, undefined, undefined)
 , ("halt",    0, undefined, undefined)
 -- , ("assert",    0, undefined, undefined)
 -- , ("retract",    0, undefined, undefined)
 ]

parseFromFile fname parser = do
    file     <- liftIO $ openFile fname ReadMode
    content  <- liftIO $ hGetContents file
    parsed   <- runParser $ fromFile fname $ withInput content $ parser
    case parsed of
        Right (ast,s) -> return ast
        Left   msgs -> processMsgs msgs

processMsgs (errs, warns) = do
    liftIO $ print $ vcat (map ppr errs)
    fail "errors occured"

--loadSource :: String -> IO (Maybe (Prog, TypeEnv), Messages)
loadSource file = do
    parsed       <- liftIO $ parseFromFile file parseSrc
    (wellformed, msgs) <- liftIO $ runTc $ wfp parsed
    case wellformed of
        Just (wfprog, env) -> do
                cp <- runDesugarT $ desugarSrc (wfprog, env)
                return (cp, env)
        Nothing -> processMsgs msgs

loadGoal inp env = do
    parsed_res  <- liftIO $ runParser $ withInput inp $ parseGoal
    parsed_goal <- case parsed_res of
                        Right (g,_) -> return g
                        Left msgs   -> processMsgs msgs
    (tcres, msgs) <- liftIO $ runTc $ withSig parsed_goal $
                              withTypeEnv env $ wfg parsed_goal
    case tcres of
        Just (tcgoal, env') -> do
            cg <- runDesugarT $ desugarGoal (tcgoal, env')
            return cg
        Nothing -> processMsgs msgs

consultFile :: FilePath -> HopesIO ()
consultFile f = do
    (src, env) <- loadSource f
    liftIO $ putStrLn ("% consulted " ++ show f ++ "")
    modify (\s -> s{ kb = KB src, currentEnv = env})

dispatch com =
    case com of
        CRefute s   ->  do
            src  <- gets kb
            env  <- gets currentEnv
            goal <- loadGoal s env
            consumeSolutions $ prove goal
        CConsult f  -> consultFile f
        CShowType p -> do
            env <- gets currentEnv
            case findTySig (liftSym p) env of
                Nothing    -> fail "undefined symbol"
                Just tysig -> liftIO $ pprint tysig
        CShowDef maybe_p -> do
            src <- gets kb
            case maybe_p of
                Nothing -> liftIO $ pprint src
                Just p  -> do
                    let cl = filter (\c -> clauseHead c == liftSym p) (clauses src)
                    liftIO $ pprint $ vcat (map ppr cl)
        CHalt -> liftIO $ bye "Leaving..."

runWith commands = runShell $ evalStateT (runWithM commands) tabulaRasa
    where tabulaRasa = HEnv mempty mempty

runWithM commands = do
  --  forM_ commands dispatch
    runLoopM

runLoopM = do
        command <- lift $ getCommand
        dispatch command -- `catch` (\e -> (liftIO $ print e))
        runLoopM

bye s     = putStrLn s >> exitWith ExitSuccess
sayYes    = putStrLn "Yes"
sayNo     = putStrLn "No"

{-
showSolutions []  = liftIO $ sayNo
showSolutions [s] = liftIO $ pprint s >> sayYes
showSolutions (s:sols) = do
    liftIO $ pprint s
    c <- liftIO $ getChar
    when (not $ c == 'q') $ showSolutions sols
-}

-- consumeSolutions :: Infer a b -> HopesIO ()
consumeSolutions i = do
    src  <- gets kb
    case infer src i of
        Nothing -> liftIO $ sayNo
        Just (a, rest) -> do
            liftIO $ sayYes
            liftIO $ print $ printanswer a
            c <- liftIO $ getChar
            when (not $ c == 'q') $ consumeSolutions rest
