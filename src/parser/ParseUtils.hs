module ParseUtils where

import HpSyn
import Types
import Loc
import Err
import Pretty
import List
import Maybe
import System.IO
import Control.Monad.State
import Control.Monad.Identity

type StringBuffer = String

data Token =
      TKoparen
    | TKcparen
    | TKgets
    | TKdot
    | TKcomma
    | TKvert
    | TKobrak
    | TKcbrak
    | TKocurly
    | TKccurly
    | TKwild
    | TKcolcol
    | TKsemi
    | TKcut
    | TKslash
    | TKbslash
    | TKarrow
    | TKid String
    | TKEOF
   deriving Eq

type Parser = StateT ParseState (ErrorT Messages Identity)
--type Parser m = StateT ParseState (ErrorT HopeError m)

data ParseState = PState {
    buffer   :: StringBuffer,
    --last_loc :: Loc,
    last_tok :: Located Token,
    cur_tok  :: Located Token,
    loc      :: Loc }
    deriving Show


getSrcBuf :: Parser StringBuffer
getSrcBuf = gets buffer

setSrcBuf :: StringBuffer -> Parser ()
setSrcBuf inp = modify (\s -> s{buffer=inp})

getSrcLoc :: Parser Loc
getSrcLoc = gets loc

setSrcLoc :: Loc -> Parser ()
setSrcLoc l = modify (\s -> s{loc=l})

setLastTok :: Located Token -> Parser ()
setLastTok t = modify (\s -> s{cur_tok=t, last_tok = (cur_tok s)})

getLastTok :: Parser (Located Token)
getLastTok = gets last_tok

getTok :: Parser (Located Token)
getTok = gets cur_tok

runParser p s = runIdentity $ runErrorT $ runStateT p s

parseFromFile p fname = do 
    file <- openFile fname ReadMode
    inp <- hGetContents file
    let result = runParser p (mkStateWithFile inp fname)
    return result


mkStateWithFile inp file = PState inp tok tok loc
    where loc = Loc file 1 1
          tok = undefined

mkState :: String -> ParseState
mkState input = mkStateWithFile input "stdin"

getName :: Located Token -> HpName
getName (L _ (TKid x)) = x
getName _ = error "not a valid token"

mkTyp :: LHpType -> Parser Type
mkTyp (L _ (HpTyGrd "o"))  = return (TyCon TyBool)
mkTyp (L _ (HpTyGrd "i"))  = return (TyCon TyAll)
mkTyp (L _ (HpTyFun t1 t2)) = do
    t1' <- mkTyp t1
    t2' <- mkTyp t2
    return (TyFun t1' t2')
mkTyp (L _ (HpTyRel t))    = do
    t' <- mkTyp t
    return (TyFun t' (TyCon TyBool))
mkTyp (L _ (HpTyTup tl))   = do
    tl' <- mapM mkTyp tl
    case tl' of
        [t] -> return t 
        _ -> return (TyTup tl')

mkTyp (L l t) = parseError (spanBegin l) (sep [quotes (ppr t), text "is not a valid type"])

mkSrc :: [LHpStmt] -> HpSource
mkSrc stmts = 
    let splitStmts (x:xs) = 
            let (cls', tys') = splitStmts xs
            in case unLoc x of 
                  HpTS ts -> (cls', ts:tys')
                  HpCl cl -> (cl:cls', tys')
        splitStmts [] = ([],[])
        (cls, tys) = splitStmts stmts
        preds = catMaybes $ map (getPred.headC) cls
        cls' = map (processCl preds) cls
        processCl vars (L loc (HpClaus h b)) =
            L loc $ HpClaus (processA vars h) (map (processA vars) b)
        processA  vars (L loc (HpAtom e)) =
            L loc $ HpAtom (processE vars e)
        processE vars (L loc (HpPar e))     = L loc (HpPar (processE vars e))
        processE vars (L loc (HpPar e))     = L loc (HpPar (processE vars e))
        processE vars (L loc (HpApp e el))  = L loc (HpApp (processE vars e) (map (processE vars) el))
        processE vars (L loc (HpAnno e t))  = L loc (HpAnno (processE vars e) t)
        processE vars (L loc (HpTerm t))    = L loc (HpTerm (processT vars t))
        processE vars le = le
        processT vars (L loc (HpId v))
            | isBound v     = L loc (HpVar v)
            | v `elem` vars = L loc (HpVar v)
            | otherwise     = L loc (HpCon v)
        processT vars (L loc (HpFun f tl)) = L loc (HpFun f (map (processT vars) tl))
        processT vars (L loc (HpList tl maybetl)) = L loc (HpList (map (processT vars) tl) (maybe maybetl (\x -> Just $ processT vars x) maybetl))
        processT vars (L loc (HpTup tl)) = L loc (HpTup (map (processT vars) tl))
        processT vars (L loc (HpSet tl)) = L loc (HpSet (map (processT vars) tl))
        processT vars lt = lt
    in  HpSrc { tysigs = tys, clauses = cls' }


parseError loc msg = throwError $ mkMsgs $ mkLocErr loc ParseError Failure msg []

instance Show Token where
    showsPrec n (TKoparen) = showString "("
    showsPrec n (TKcparen) = showString ")"
    showsPrec n (TKgets)   = showString ":-"
    showsPrec n (TKdot)    = showString "."
    showsPrec n (TKcomma)  = showString ","
    showsPrec n (TKvert)   = showString "|"
    showsPrec n (TKobrak)  = showString "["
    showsPrec n (TKcbrak)  = showString "]"
    showsPrec n (TKocurly) = showString "{"
    showsPrec n (TKccurly) = showString "}"
    showsPrec n (TKwild)   = showString "_"
    showsPrec n (TKcolcol) = showString "::"
    showsPrec n (TKsemi)   = showString ";"
    showsPrec n (TKcut)    = showString "!"
    showsPrec n (TKslash)  = showString "/"
    showsPrec n (TKbslash) = showString "\\"
    showsPrec n (TKarrow)  = showString "->"
    showsPrec n (TKid s)   = showString s

type LTok = Located Token

mkLTk = mkLoc

instance Pretty Token where
    ppr t = text (show t)
