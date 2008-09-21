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

module ParseUtils where

import Syntax
import Lang
import Types
import Loc
import Error
import Pretty
import Buildins
import Char (isUpper, isDigit)
import List (partition, nub)
import Control.Monad.State

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
    | TKsq
    | TKeq
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
    showsPrec n (TKsq)     = showString "'"
    showsPrec n (TKeq)     = showString "="


instance Pretty Token where
    ppr t = text (show t)


type ParserT m = StateT ParseState (ErrorT Messages m)
-- type Parser    = ParserT IO

data ParseState = PState { pinput :: ParserInput,
                           ptok   :: Located Token
                         }

instance (Monad m) => MonadLoc (ParserT m) where
    getLocSpan = gets (locSpan . ptok)


runParser p = runErrorT $ runStateT p s
    where s = PState "" t
          t = located (Loc "stdin" 1 1) TKBOF

getInput :: Monad m => ParserT m ParserInput
getInput = gets pinput

setInput :: Monad m => ParserInput -> ParserT m ()
setInput inp = modify (\s -> s{ pinput = inp })

setTok :: Monad m =>  Located Token -> ParserT m ()
setTok tok = modify (\s -> s{ ptok = tok })

parseErrorWithLoc :: Monad m => Loc -> ErrDesc -> ParserT m a
parseErrorWithLoc loc msg = 
    throwError $ mkMsgs $ mkErrWithLoc loc ParseError Failure msg

parseError :: Monad m => ErrDesc -> ParserT m a
parseError msg = do
    l <- getLoc
    parseErrorWithLoc l msg

parseError' :: Monad m =>  ParserT m a
parseError' = do
    tok <- gets ptok
    case unLoc tok of
        TKEOF -> parseError $ text "unexpected end of input"
        _     -> parseError $ sep [ text "parse error on input",
                                    quotes (ppr (unLoc tok)) ]

fromFile name m = do
    let initLoc = Loc name 1 1
        initTok = located initLoc TKBOF
    modify (\s -> s {ptok = initTok})
    m

withInput inp m = setInput inp >> m

tokSym :: Located Token -> HpSymbol
tokSym t = Sym (tokId t)

tokId :: Located Token -> String
tokId (L _ (TKid x)) = x
tokId _ = error "not a valid token"

type HpStmt a = Either (LHpClause a) (TySig a)

--mkSrc :: [HpStmt a] -> Parser (HpProg a)
mkSrc stmts = 
    let (l, r) = collectEither stmts
    in  return HpSrc { clauses = l, tyEnv = r }

collectEither :: [Either a b] -> ([a], [b])
collectEither es = (map unL l, map unR r)
    where isLeft (Left _) = True
          isLeft _ = False
          unL (Left a)  = a
          unR (Right a) = a
          (l, r) = partition isLeft es

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
        symbols'' (HpSym AnonSym)  = []
        symbols'' (HpSym s)    = [s]
        symbols'' (HpTup es)   = concatMap symbols' es
        symbols'' (HpAnn e t)  = symbols'' (unLoc e)
    in  (HpClause vars' xs ys)

mkLambda x (L _ (HpLam ys e'))  = HpLam ((HpBind (liftSym (tokId x)) bogusType):ys) e'
mkLambda x e = HpLam [(HpBind (liftSym (tokId x)) bogusType)] e

mkList elems tl = 
    unLoc $ foldr (\x -> \y -> located x $ HpApp consE [x,y]) lastel elems
    where consE   = located bogusLoc $ HpSym (mkBuildin ".")
          lastel  = case tl of
                        Nothing -> located bogusLoc $ HpSym (mkBuildin "[]")
                        Just e  -> e

-- put some hardcoded building numerics
mkSym s
    | all isDigit (show (unLoc s)) = unLoc $ mkInt (read (show (unLoc s)))
    | isBuildin (show (unLoc s))   = HpSym $ mkBuildin (show (unLoc s))
    | otherwise             = HpSym (tokSym s)

mkInt 0 = located bogusLoc $ HpSym (mkBuildin "0")
mkInt i = located bogusLoc $ HpApp (located bogusLoc $ HpSym (mkBuildin "s")) [minus_one]
    where minus_one = mkInt (i-1)


data HpType   =
      HpTyGrd String           -- ground type
    | HpTyFun LHpType LHpType  -- type of function
    | HpTyTup [LHpType]        -- type of tuple
    | HpTyRel LHpType          -- type of relation /isomorfic to a function type

type LHpType   = Located HpType

mkTyp :: Monad m => LHpType -> ParserT m Type
mkTyp (L _ (HpTyGrd "o"))  = return tyBool
mkTyp (L _ (HpTyGrd "i"))  = return tyAll
mkTyp (L _ (HpTyFun t1 t2)) = do
    t1' <- mkTyp t1
    t2' <- mkTyp t2
    return (TyFun t1' t2')
mkTyp (L _ (HpTyRel t))    = do
    t' <- mkTyp t
    return (TyFun t' tyBool)

{-
mkTyp (L _ (HpTyTup tl))   = do
    tl' <- mapM mkTyp tl
    case tl' of
        [t] -> return t 
        _ -> return (TyTup tl')
-}

mkTyp (L l t) = parseErrorWithLoc (spanBegin l) (text "Not a valid type")

