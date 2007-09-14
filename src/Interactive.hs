module Interactive where

import IO
import System
import System.IO
import System.Console.Readline
import Control.Monad.State
import List (isPrefixOf)

import Syntax
import Parser
import Wellform
import Typecheck
import Tc

import Pretty
import Err
import Loc
import Types

import Core
import Infer
import Hopl


type HopeI = StateT HopeEnv IO

data HopeEnv = 
    HEnv {  
        currentEnv   :: TypeEnv,
        consultedSrc :: Prog (HpSymbol, Type)
    }

data UserCommand =
      RefuteGoal  String
    | ConsultSrc  FilePath
    | ShowType    String
    | ShowHelp
    | Quit


-- type HopeI = ReaderT (HopeEnv) IO
-- filename can have leading and trailing whitespaces
parseUserCommand (':':'l':r) = return $ ConsultSrc (dropWhile (==' ') r)
parseUserCommand (':':'t':r) = return $ ShowType   (dropWhile (==' ') r)
parseUserCommand (':':'q':r) = return $ Quit
parseUserCommand (':':'h':r) = return $ ShowHelp
parseUserCommand (':':r)     = fail "Unknown command. Type :h for help."
parseUserCommand str         = return $ RefuteGoal str


commandCompletionFunction env xs = goalCompletionFunction env xs
goalCompletionFunction = predicateCompletionFunction

predicateCompletionFunction env p = 
    let vs =  map show [ v | (v,_) <- (currentEnv env) ]
    in  do
        return $ filter (p `isPrefixOf`) vs

runInteract act = evalStateT (interactLoop act) initEnv
    where initEnv = HEnv [] []

action (RefuteGoal s) = do
    src  <- gets consultedSrc
    env  <- gets currentEnv
    goal <- loadGoal s env
    showSolutions $ runInfer src (prove goal)

action (ConsultSrc f) = consultFile f

action (ShowType p) = do
    env <- gets currentEnv
    case lookup (Sym p) env of
        Nothing -> fail "undefined symbol"
        Just ty -> liftIO $ pprint (sep [ ppr p, text "::", ppr ty])

action Quit = liftIO $ bye

action _    = fail "functionality not implemented"

consultFile :: FilePath -> HopeI ()
consultFile f = do
    (src, env) <- loadSource f
    modify (\s -> s{ consultedSrc = src, currentEnv = env})

showHelp  = putStrLn "no help"

bye       = putStrLn "Bye" >> exitWith ExitSuccess
sayYes    = putStrLn "Yes"
sayNo     = putStrLn "No"

promptStr = "-? "

interactLoop :: (UserCommand -> HopeI ()) -> HopeI ()
interactLoop act = do
    env <- get
    liftIO $ setCompletionEntryFunction (Just (commandCompletionFunction env))
    maybe_inp <- liftIO $ readline promptStr
    case maybe_inp of
        Nothing -> interactLoop act
        Just "" -> interactLoop act
        Just str -> do
            liftIO $ addHistory str
            (parseUserCommand str >>= act) `catchError` (\e -> (liftIO $ print e))
            interactLoop act

showSolutions []  = liftIO $ sayNo
showSolutions [s] = liftIO $ pprint s >> sayYes
showSolutions (s:sols) = do
    liftIO $ pprint s
    c <- liftIO $ getChar
    when (not $ c == 'q') $ showSolutions sols



parseFromFile :: MonadIO m => FilePath -> ParserT m a -> m a
parseFromFile fname parser = do 
    file <- liftIO $ openFile fname ReadMode
    inp  <- liftIO $ hGetContents file
    parsed_res <- runParser $ fromFile fname $ withInput inp $ parser
    case parsed_res of
        Right (p,s) -> return p
        Left   msgs -> processMsgs msgs


processMsgs (errs, warns) = do
    liftIO $ print $ vcat (map ppr errs)
    fail "errors occured"

--loadSource :: String -> IO (Maybe (Prog, TypeEnv), Messages)
loadSource file = do
    parsed_prog   <- liftIO $ parseFromFile file parseSrc
    (tcres, msgs) <- liftIO $ runTc $ wfp parsed_prog
    case tcres of
        Just (tcprog, env) -> do
                cp <- runCore $ ctProg (tcprog, env)
                return (cp, env)
        Nothing -> processMsgs msgs

loadGoal inp env = do
    parsed_res  <- liftIO $ runParser $ withInput inp $ parseGoal
    parsed_goal <- case parsed_res of
                        Right (g,_) -> return g
                        Left msgs   -> processMsgs msgs
    (tcres, msgs) <- liftIO $ runTc $ withAllDefined parsed_goal $ withTypeEnv env $ wfg parsed_goal
    case tcres of
        Just (tcgoal, env') -> do
            cg <- runCore $ ctGoal (tcgoal, env')
            return cg
        Nothing -> processMsgs msgs
