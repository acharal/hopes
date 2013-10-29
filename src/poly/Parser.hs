--  Copyright (C) 2013 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--                     Emmanouil Koukoutos   <manoskouk@softlab.ntua.gr>
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


{-
 - Parser module for polyHOPES
 -}

{-
 - KNOWN BUGS:
 -  prefix operator used as atom fails
 -}

module Parser (runParser, parseSrc, emptyParseState, getState, parseHopes2) where

import qualified Lexer as L
import Syntax
import qualified Operator as Operators

import Text.Parsec hiding (runParser)
import Text.Parsec.Expr
import Text.Parsec.Pos
import Data.Monoid (mappend)
import Control.Monad (when)

import Control.Monad.IO.Class
import Loc


-- type OperatorTable s u m a = [[Operator s u m a]]

-- The user state in the parser. Contains an operator table as well as a 
-- cached operator table as Text.Parsec.Expr demands it
data ParseState s m = 
    ParseSt { operatorTable :: Operators.OperatorTable
            , cachedTable   :: [(Int, [Operator s (ParseState s m) m ( SExpr LocSpan )])]
            } 

-- The empty parser state
emptyParseState = ParseSt [] []

-- A ParsecT type with user state as defined above
type ParserT s m a = ParsecT s (ParseState s m) m a



instance HasLocation SourcePos where
    loc pos = Loc (sourceName pos) (sourceLine pos) (sourceColumn pos)

{- 
 - Utility parsing functions
 -}

-- Takes a functor and a list of argument lists with positions,
-- and makes a compatible nested application
nestedApp :: SExpr LocSpan                  -- Functor
          -> [([SExpr LocSpan], SourcePos)] -- Argument lists with their positions
          -> SExpr LocSpan                  -- A nested application compatible with input
nestedApp = 
    foldl (\fun (args, pos) -> SExpr_app (mkSpan (spanBegin $ locSpan fun) pos) fun args)

-- Utility function which parses comma- separated arguments in parentheses
-- Returns args, along with their ENDING position (after last ")")
args :: Stream s m Char => ParserT s m ([SExpr LocSpan] , SourcePos )
args = do symbol "("
          argsSet <- commaSep1 (try argExpr <|> (allExpr False) ) -- False disallows ',' in lambda
          pos2 <- getPosition  
          symbol ")"
          return ( argsSet, incSourceColumn pos2 1 )


{-
 - Utility functions to easily create expressions and other syntactic structures
 -}

-- Constant expression
mkConstEx s i p1 p2 = SExpr_const (mkSpan p1 p2) (Const (mkSpan p1 p2) s) False i (-1) 

-- Constant
mkConst c p1 p2 = Const (mkSpan p1 p2) c

-- Predicate constant
mkPredCon s i p1 p2 p3 p4 = SExpr_predCon (mkSpan p1 p2) (Const (mkSpan p3 p4) s) i (-1)

-- Variable
mkVar s p1 p2 = if s=="_" then AnonVar (mkSpan p1 p2) else Var (mkSpan p1 p2) s

-- Variable expression
mkVarEx s p1 p2 = SExpr_var (mkSpan p1 p2) (mkVar s p1 p2) False

-- Numeric expression
mkNum num p1 p2 = SExpr_number (mkSpan p1 p2) num

-- List
mkList hds tl p1 p2 = SExpr_list (mkSpan p1 p2) hds tl

-- Lambda abstraction
mkLam vars bd p1 p2 = SExpr_lam (mkSpan p1 p2) vars bd

-- Operator expression
mkOpExpr fixity op [arg] = SExpr_op span op fixity False [arg] 
    where span = locSpan op `mappend` locSpan arg
mkOpExpr fixity op args@([arg1, arg2]) = SExpr_op span op fixity False args
    where span = locSpan arg1 `mappend` locSpan arg2

-- Inverse follows symbol
mkGets :: String -> SGets
mkGets "<-" = SGets_poly
mkGets ":-" = SGets_mono
mkGets _    = error "gets" -- TODO some better error discipline

-- Clause
mkClause hd bd p1 p2 = SClause (mkSpan p1 p2) hd bd


{-
 - Lexers, imported from Lexer module
 -}

commaSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep  = L.commaSep1 L.hopes

commaSep1 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep1 = L.commaSep1 L.hopes

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = L.parens L.hopes

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme = L.lexeme L.hopes

identifier :: Stream s m Char => ParsecT s u m String
identifier = L.identifier L.hopes

dot :: Stream s m Char => ParsecT s u m String
dot = L.dot L.hopes

inf :: Stream s m Char => ParsecT s u m String
inf = L.symbol L.hopes ":-"

stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = L.stringLiteral2 L.hopes

brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = L.brackets L.hopes

varIdent :: Stream s m Char => ParsecT s u m String
varIdent = L.varIdent L.hopes

-- Constant or graphic constant/operator
conIdent :: Stream s m Char => ParsecT s u m String
conIdent = L.conIdent L.hopes

natural :: Stream s m Char => ParsecT s u m Integer
natural = L.natural L.hopes

-- Atom is either a constant, or a string literal, or ',' atom
atom :: Stream s m Char => ParsecT s u m String
atom = choice [try conIdent, try stringLiteral, symbol ","] -- <?> "atom"

naturalOrFloat :: Stream s m Char => ParsecT s u m (Either Integer Double)
naturalOrFloat = L.naturalOrFloat L.hopes

integer :: Stream s m Char => ParsecT s u m Integer
integer = L.integer L.hopes

float :: Stream s m Char => ParsecT s u m Double
float = L.float L.hopes

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = L.symbol L.hopes

-- reserved keyword
predTK :: Stream s m Char => ParsecT s u m ()
predTK = L.reserved L.hopes "pred"
{-
trueTK :: Stream s m Char => ParsecT s u m ()
trueTK = L.reserved L.hopes "true"

failTK :: Stream s m Char => ParsecT s u m ()
failTK = L.reserved L.hopes "fail"
-}



{-
 - Parser 
 - 
 - TODO : implement optional arities in constants/ 
 -        poymorphic clause heads
 - TODO : implement type annotations
 - TODO : improve position recording where constants/atoms are involved
 -}

-- Basic structures

variable :: Stream s m Char => ParserT s m (SExpr LocSpan)
variable = try ( do { pos1 <- getPosition
                    ; s    <- varIdent
                    ; return $ mkVarEx s pos1 (updatePosString pos1 s)
                    } -- <?> ("variable")
               )


constant :: Stream s m Char => ParserT s m (SExpr LocSpan)
constant = try ( do { pos1 <- getPosition
                    ; c    <- atom 
                    ; pos2 <- getPosition
                    ; return $ mkConstEx c Nothing pos1 pos2
                    } -- <?> ("constant")
               )

predConst :: Stream s m Char => ParserT s m (SExpr LocSpan)
predConst = try ( do  { pos1 <- getPosition
                      ; predTK
                      ; pos2 <- getPosition
                      ; s <- atom
                      ; pos3 <- getPosition
                      ; n <- optionMaybe $ do 
                                 symbol "/"
                                 n' <- natural
                                 return $ fromIntegral n'
                      ; pos4 <- getPosition
                      ; return $ mkPredCon s n pos1 pos4 pos2 pos3
                      } -- <?> ("predicate constant")
                )

numberExpr :: Stream s m Char => ParserT s m (SExpr LocSpan)
numberExpr = try $ do { pos1 <- getPosition
                      ; n <- try ( do n <- float -- float is longer so try first
                                      return $ Right n 
                                 ) <|>    
                             do n <- natural
                                return $ Left n 
                                   
                      ; let pos2 = incSourceColumn pos1 (case n of 
                                                             Left  n' -> length $ show n'
                                                             Right n' -> length $ show n'
                                                        )
                      ; return $ mkNum n pos1 pos2
                      }


cut :: Stream s m Char => ParserT s m (SExpr LocSpan)
cut = do { pos1 <- getPosition
         ; symbol "!"
         ; return $ mkConstEx "!" Nothing pos1 (incSourceColumn pos1 1)
         }

-- More complex structures

list :: Stream s m Char => ParserT s m (SExpr LocSpan)
list = (try listEmpty) <|> (try listNonEmpty) -- <?> ("list")  
    where listEmpty = do { pos1 <- getPosition
                         ; symbol "[]"
                         ; return $ mkList [] Nothing pos1 (incSourceColumn pos1 2)
                         }
          listNonEmpty = do { pos1 <- getPosition 
                            ; symbol "["
                            ; es <- listAtoms
                            ; tl <- optionMaybe tail
                            ; pos2 <- getPosition
                            ; symbol "]"
                            ; return $ mkList es tl pos1 (incSourceColumn pos2 1)
                           {- ; (hds, optTl) <- brackets $ do 
                                  es <- listatoms
                                  tl <- optionMaybe tail
                                  return (es, tl) 
                            ; pos2 <- getPosition
                            ; return $ mkList hds optTl pos1 pos2 -}
                            }  
          listAtoms = commaSep1 argExpr
          tail      = do { symbol "|"
                         ; variable <|> list 
                           -- Comment: This disallows irregular lists
                         }
        
-- Parameter true -> ',' is allowed in body (we are not in an argument)
lambda :: Stream s m Char => Bool -> ParserT s m (SExpr LocSpan)
lambda isFull = try ( do 
    { pos1 <- getPosition
    ; symbol "\\~" 
    ; vars <- parens $ commaSep1 $ varLit -- <?> "lambda variables"
    ; symbol "=>"
    ; ex   <- try (if isFull then fullExpr else argExpr) <|> 
              (allExpr isFull) 
    ; return $ mkLam vars ex pos1 (spanEnd $ locSpan ex)
    } ) --  <?> "lambda"
    where varLit = do { pos1 <- getPosition
                      ; var  <- varIdent 
                      ; return $ mkVar var pos1 (updatePosString pos1 var)
                      }


-- Application tries to parse an atomic expression applied on
-- any number of sets of arguments.
-- If there are no arguments, just the atomic expression is
-- parsed.
-- This along with "lambda" and expressions with operators
-- describe all expressions.
application :: Stream s m Char => ParserT s m (SExpr LocSpan)
application  = try ( do 
    { s    <- atomicExpr
    ; as   <- many args -- as :: [[SExpr LocSpan]]  
    ; case as of 
        [] -> return s
        _  -> return $ nestedApp s as
    } -- <?> ("application")
    )
                  

inParens :: Stream s m Char => ParserT s m (SExpr LocSpan)
inParens = try $ do
    ex <- parens $ fullExpr <|> (allExpr True)
    return $ SExpr_paren (getInfo ex) ex
    
-- Everything except application or lambda. Functions as head
-- of application
atomicExpr :: Stream s m Char => ParserT s m (SExpr LocSpan)
atomicExpr = choice [ predConst
                    , list
                    , constant
                    , variable
                    , numberExpr
                    , cut
                    , inParens 
                    ] -- <?> ("atomicExpr")

-- Used as an argument to expr to create expr. parser
-- Boolean parameter says if fullExpr is allowed in lambda body
allExpr :: Stream s m Char => Bool -> ParserT s m (SExpr LocSpan)
allExpr fullLam = try (lambda fullLam) <|> application


-- Expressions with operators, built from the operator table
-- with buildExpressionParser
-- TODO: better error messages when failing to parse operator

-- Expression for arguments (no ',' operator allowed)
argExpr :: Stream s m Char => ParserT s m (SExpr LocSpan)
argExpr = do { st <- getState
             ; let opTbl = map snd $ cachedTable st
             ; buildExpressionParser opTbl (allExpr False)
             } -- <?> "operator"

-- The precedence of the ',' operator
commaPrec = 1000

-- General expressions (',' operator allowed)
fullExpr :: Stream s m Char => ParserT s m (SExpr LocSpan)
fullExpr = do { -- Grab state and add ',' operator
                st <- getState
              ; let opTbl = map snd cachedTable''
                        where cachedTable'  = cachedTable st
                              -- Find the right spot to put ','
                              (t1, t2) = span ((<commaPrec).fst) cachedTable'
                              cachedTable'' = case t2 of 
                                  []        -> t1 ++ [ (commaPrec, [commaOp])]
                                  (hd2:tl2) -> if fst hd2 == commaPrec 
                                                 then t1 ++ (commaPrec, commaOp:snd hd2) : tl2
                                                 else t1 ++ (commaPrec, [commaOp]) : t2
                              -- ',' must get special treatment because it is not a graphic token
                              commaOp = Infix (try $ do { pos1 <- getPosition
                                                        ; symbol ","
                                                        ; let pos2 = incSourceColumn pos1 1
                                                        ; return $ (\x y-> mkOpExpr False (mkConst "," pos1 pos2) [x,y])
                                                        }
                                              ) AssocLeft
                -- Now build an expression parser with the new operator table
              ; buildExpressionParser opTbl (allExpr True)
              } -- <?> "operator"


-- Head of a clause 
-- Constant with arity (for polymorphic <-) or Prolog-style head
headCl :: Stream s m Char => ParserT s m (SExpr LocSpan)
headCl = ( try $ do pos1 <- getPosition
                    c    <- atom
                    symbol "/"
                    pos2 <- getPosition
                    n    <- natural
                    let pos2' = incSourceColumn pos2 (length $ show n)
                    return $ mkConstEx c (Just $ fromIntegral n) pos1 pos2'
         ) <|>
         ( try $ do pos1 <- getPosition
                    c    <- atom 
                    pos2 <- getPosition 
                    as   <- many args 
                    let c' = mkConstEx c Nothing pos1 pos2
                    return $ nestedApp c' as 
         )

-- Clause
clause :: Stream s m Char => ParserT s m (SSent LocSpan)
clause = do pos1 <- getPosition
            h <- headCl
            b <- optionMaybe $ do 
                gets <- symbol ":-" <|> symbol "<-" -- mono or poly?
                body <- (try fullExpr) <|> (allExpr True)
                return (mkGets gets, body)
            pos2 <- getPosition
            symbol "." -- <?> "dot"
            let pos2' = incSourceColumn pos2 1
            return $ SSent_clause $ SClause (mkSpan pos1 pos2') h b


{-
 - Here follow the types of acceptable sentences.
 - Currently only working with clauses.
 - TODO : add
 -  - Goals
 -}

-- Directives
command :: Stream s m Char => ParserT s m (SSent LocSpan)
command = do { pos1 <- getPosition 
             ; symbol ":-"
             ; ex <- try fullExpr <|> (allExpr True)
             ; pos2 <- getPosition
             ; symbol "."
             ; let pos2' = incSourceColumn pos2 1
             ; return $ SSent_comm $ SCommand (mkSpan pos1 pos2') ex
             }

-- Goals
goal :: Stream s m Char => ParserT s m (SSent LocSpan)
goal = do { pos1 <- getPosition
          ; symbol "?-"
          ; ex <- try fullExpr <|> (allExpr True)
          ; pos2 <- getPosition
          ; symbol "."
          ; let pos2' = incSourceColumn pos2 1
          ; return $ SSent_comm $ SCommand (mkSpan pos1 pos2') ex
          }

-- Sentence
sentence :: Stream s m Char => ParserT s m (SSent LocSpan)
sentence = choice [ try goal
                  , try command
                  , clause
                  ] -- <?> ("sentence") 

-- | Directives must be only in goal clause (without head literal)
-- Only recognizing op directives thus far. TODO add more
opDirective1 (SSent_comm (SCommand _ e)) = opDirective e
opDirective1 _ = return ()

opDirective ( SExpr_app _ (SExpr_const _ (Const _ "op") _ _ _ ) 
                            -- TODO improve the last args to None (Just 3) ?? ) 
            [ SExpr_number _ (Left p)
            , SExpr_const _ (Const _ a) _ _ _
            , SExpr_const _ (Const _ n) _ _ _
            ]) = do
    updateState (\st -> st{ operatorTable = (t st)})
    cached <- buildOpTable
    updateState (\st -> st{ cachedTable = cached })
    where t st = Operators.updateOpTable (operatorTable st) op
          op   = mkOp ((fromInteger p)::Int, a, n)
          mkOp (p, a, n) = (p, Operators.Operator n a)

opDirective _ = return ()


-- Build operator table, save it in monadic state
buildOpTable :: Stream s m Char => ParsecT s (ParseState s m) m [(Int, [Operator s (ParseState s m) m (SExpr LocSpan)])]
buildOpTable = do { st <- getState
                  ; return $ mkOpTable (operatorTable st)
                  }
    where mkOpTable ops = map (\(p, op) -> (p, map opMap op)) $ Operators.groupByPrec ops
              where 
                  opMap op | Operators.isPrefixOp  op = prefixOp  (Operators.opName op)
                           | Operators.isPostfixOp op = postfixOp (Operators.opName op)
                           | otherwise                = infixOp   (Operators.opName op) (assocMap op)
                      where 
                            -- Pass precedence to the operator expressions to store it in the synax tree
                            infixOp   name assoc = Infix   (oper (\n -> \x -> \y -> mkOpExpr False n [x,y]) name) assoc
                            prefixOp  name       = Prefix  (oper (\n -> \x -> mkOpExpr True  n [x]) name) 
                            postfixOp name       = Postfix (oper (\n -> \x -> mkOpExpr False n [x]) name)
                            assocMap op | Operators.isAssocLeft  op = AssocLeft
                                        | Operators.isAssocRight op = AssocRight
                                        | otherwise                 = AssocNone

                  oper f name = try $ do { pos1 <- getPosition
                                         ; op   <- atom;
                                         ; pos2 <- getPosition
                                         ; if op == name then return (f $ mkConst name pos1 pos2) else fail ""
                                         }


-- top-level parsing
-- TODO: Parse operators in head
-- TODO: Parse predicates of form module:predicate.
-- TODO: Attach position to AST
-- TODO: Parse module directives
-- TODO: Parse include Directives

runParser :: Stream s m t =>
     ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runParser = runPT

parseSrc :: Stream s m Char => ParserT s m [SSent LocSpan]
parseSrc = do 
    L.whiteSpace L.hopes
    sents <- many sentence'
    eof
    return sents
    where sentence' = do
              s <- sentence
              when (isCommand s) (opDirective1 s)  
              return s

-- Wrapper function for runP. Returns both parsed program and final state
runHopesParser p st sourcename input = runP p' st sourcename input
    where p' = do 
            L.whiteSpace L.hopes
            res <- p
            st' <- getState
            return (res, st')



-- Parse after reading operators
parseHopes2 input = do
    ops <- readFile "../../pl/op.pl"
    (p, optable) <- parse st' "pl/op.pl" ops
    f <- readFile input
    result <- parse optable input f
    return result
    where st' = ParseSt initOps []
          initOps = [] -- No initial operators needed
          parse st src input = case runHopesParser (buildTable >> do { sents <- many1 sentence' 
                                                                     ; eof
                                                                     ; return sents
                                                                     }
                                                    ) st src input of
                                   Left err ->  do putStr "parse error at "
                                                   print err
                                                   fail ""
                                   Right res -> return res
          sentence' = do 
              s <- sentence
              when (isCommand s) (opDirective1 s)
              return s
          buildTable = do 
              cached <- buildOpTable
              updateState (\st -> st{ cachedTable = cached })

-- Parse without reading operators        
parseHopes p input = 
    case runHopesParser p st "" input of 
        Left err -> do putStr "parse error at " 
                       print err
        Right (x,_) -> print x
    where st = ParseSt [] []


