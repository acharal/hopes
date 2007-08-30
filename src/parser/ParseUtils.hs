module ParseUtils where

import Syntax
import Types
import Loc
import Err
import Pretty (Pretty(..), text)

import Char (isUpper)
import List (partition, nub)

import System.IO
import Control.Monad.State    (StateT, gets, modify, runStateT)
import Control.Monad.Identity (Identity, runIdentity)

type ParserInput = String

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
    | TKBOF
   deriving Eq

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


instance Pretty Token where
    ppr t = text (show t)


type Parser = StateT ParseState (ErrorT Messages Identity)

data ParseState = PState { pinput :: ParserInput,
                           ptok   :: Located Token
                         }

instance MonadLoc Parser where
    getLocSpan = gets (locSpan . ptok)


runParser p s = runIdentity $ runErrorT $ runStateT p s

getInput :: Parser ParserInput
getInput = gets pinput

setInput :: ParserInput -> Parser ()
setInput inp = modify (\s -> s{ pinput = inp })

setTok :: Located Token -> Parser ()
setTok tok = modify (\s -> s{ ptok = tok })

parseErrorWithLoc loc msg = 
    throwError $ mkMsgs $ mkErrWithLoc loc ParseError Failure msg

parseError msg = do
    l <- getLoc
    parseErrorWithLoc l msg


parseFromFile p fname = do 
    file <- openFile fname ReadMode
    inp <- hGetContents file
    let result = runParser p (mkStateWithFile inp fname)
    return result

mkStateWithFile inp file = PState inp t
    where l = Loc file 1 1
          t = located l TKBOF

mkState :: String -> ParseState
mkState input = mkStateWithFile input "stdin"

tokSym :: Located Token -> HpSymbol
tokSym t = Sym (tokId t)

tokId :: Located Token -> String
tokId (L _ (TKid x)) = x
tokId _ = error "not a valid token"

type HpStmt a = Either (LHpFormula a) (LHpTySign a)

mkSrc :: [HpStmt a] -> Parser (HpProg a)
mkSrc stmts = 
    let (l, r) = collectEither stmts
    in  return HpProg { clauses = l,  tysigs' = r }

collectEither :: [Either a b] -> ([a], [b])
collectEither es = (map unL l, map unR r)
    where isLeft (Left _) = True
          isLeft _ = False
          unL (Left a)  = a
          unR (Right a) = a
          (l, r) = partition isLeft es

bogusType :: Type
bogusType = error ("This type is a placeholder and must not be evaluated")

isCapitalized (Sym s) = isUpper $ head s


--mkClause :: LHpExpr a -> [LHpExpr a] -> HpClause a
mkQuantForm xs ys = 
    let syms   = concatMap symbols' (xs ++ ys)
        bind x = HpBind x bogusType
        vars' = map bind $ nub $ filter isCapitalized syms
        symbols' le = symbols'' (unLoc le)
        symbols'' (HpPar e)    = symbols' e
        symbols'' (HpLam _ e)  = symbols' e
        symbols'' (HpApp e es) = concatMap symbols' (e:es)
        symbols'' (HpSym s)    = [s]
        symbols'' (HpTup es)   = concatMap symbols' es
        symbols'' (HpWildcat)  = []
    in  (HpForm vars' xs ys)

mkList elems tl = 
    unLoc $ foldr (\x -> \y -> located x $ HpApp consE [x,y]) nilE elems
    where consE = located bogusLoc consSym
          nilE  = located bogusLoc nilSym

mkSym = HpSym . tokSym

data HpType   =
      HpTyGrd String                    -- ground type
    | HpTyFun LHpType LHpType           -- type of function
    | HpTyTup [LHpType]                 -- type of tuple
    | HpTyRel LHpType                   -- type of relation / isomorfic to a function type

type LHpType   = Located HpType

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

mkTyp (L l t) = parseErrorWithLoc (spanBegin l) (text "Not a valid type")

