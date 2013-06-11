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

module Parser where

import Basic
import qualified Lexer as L
import Syntax
import qualified Operator as Operators
import Text.Parsec
import Text.Parsec.Expr
import Pos
import Data.Char
import Data.List 
import Control.Monad (when)

import qualified Data.ByteString.Char8 as C
import Text.Parsec.ByteString

-- type OperatorTable s u m a = [[Operator s u m a]]

data ParseState s m = 
    ParseSt { operatorTable :: Operators.OperatorTable
            , cachedTable   :: [(Int, [Operator s (ParseState s m) m ( SExpr PosSpan )])]
            } 

type ParserT s m a = ParsecT s (ParseState s m) m a


-- helpers

followedBy p after = do { rv <- p; after; return rv }

-- |  opposite of take
skip n [] = []
skip 0 x = x
skip n (x:xs) = skip (n-1) xs

-- Takes a functor and a list of argument lists with positions,
-- and makes a compatible nested application
nestedApp :: SExpr PosSpan                  -- Functor
          -> [([SExpr PosSpan], SourcePos)] -- Argument lists with their positions
          -> SExpr PosSpan                  -- A nested application compatible with input
nestedApp = 
    foldl (\fun (args, pos) -> SExpr_app (mkSpan (spanBegin $ posSpan fun) pos) fun args)


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
mkVarEx s p1 p2 = SExpr_var (mkSpan p1 p2) $ mkVar s p1 p2

-- Numeric expression
mkNum num p1 p2 = SExpr_number (mkSpan p1 p2) num

-- List
mkList hds tl p1 p2 = SExpr_list (mkSpan p1 p2) hds tl

-- Lambda abstraction
mkLam vars bd p1 p2 = SExpr_lam (mkSpan p1 p2) vars bd

-- Operator expression
mkOpExpr op [arg] = SExpr_op span op False [arg] 
    where span = posSpan op `mappend` posSpan arg
mkOpExpr op args@([arg1, arg2]) = SExpr_op span op False args
    where span = posSpan arg1 `mappend` posSpan arg2

-- Inverse follows symbol
mkGets :: String -> SGets
mkGets "<-" = SGets_poly
mkGets ":-" = SGets_mono
mkGets _    = error "gets" -- TODO some better error discipline

-- Clause
mkClause hd bd p1 p2 = SClause (mkSpan p1 p2) hd bd


-- Lexer 

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

atom :: Stream s m Char => ParsecT s u m String
atom = try conIdent <|> stringLiteral -- <?> "atom"

naturalOrFloat :: Stream s m Char => ParsecT s u m (Either Integer Double)
naturalOrFloat = L.naturalOrFloat L.hopes

integer :: Stream s m Char => ParsecT s u m Integer
integer = L.integer L.hopes

float :: Stream s m Char => ParsecT s u m Double
float = L.float L.hopes

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = L.symbol L.hopes


--reserved
predTK :: Stream s m Char => ParsecT s u m ()
predTK = L.reserved L.hopes "pred"

trueTK :: Stream s m Char => ParsecT s u m ()
trueTK = L.reserved L.hopes "true"

failTK :: Stream s m Char => ParsecT s u m ()
failTK = L.reserved L.hopes "fail"




-- parser 
-- TODO : implement optional arities in constants/ 
--        poymorphic clause heads
-- TODO : implement type annotations

-- Basic structures

variable :: Stream s m Char => ParserT s m (SExpr PosSpan)
variable = try ( do { pos1 <- getPosition
                    ; s    <- varIdent
                    ; pos2 <- getPosition
                    ; return $ mkVarEx s pos1 pos2
                    } -- <?> ("variable")
               )


constant :: Stream s m Char => ParserT s m (SExpr PosSpan)
constant = try ( do { pos1 <- getPosition
                    ; c    <- atom 
                    ; pos2 <- getPosition
                    ; return $ mkConstEx c Nothing pos1 pos2
                    } -- <?> ("constant")
               )

predConst :: Stream s m Char => ParserT s m (SExpr PosSpan)
predConst = try ( do  { pos1 <- getPosition
                      ; predTK
                      ; pos2 <- getPosition
                      ; s <- atom
                      ; pos3 <- getPosition
                      ; n <- optionMaybe $ do 
                             char '/'
                             n' <- natural
                             return $ fromIntegral n'
                      ; pos4 <- getPosition
                      ; return $ mkPredCon s n pos1 pos4 pos2 pos3
                      } -- <?> ("predicate constant")
                )

numberExpr :: Stream s m Char => ParserT s m (SExpr PosSpan)
numberExpr = try $ do { pos1 <- getPosition
                      ; n <- choice [ try $ do { n <- float -- float is longer so try first
                                               ; return $ Right n 
                                               } 
                                    , do { n <- natural
                                         ; return $ Left n 
                                         }
                                    ]
                      ; pos2 <- getPosition
                      ; return $ mkNum n pos1 pos2
                      }


cut :: Stream s m Char => ParserT s m (SExpr PosSpan)
cut = do { pos1 <- getPosition
         ; L.symbol L.hopes "!"
         ; pos2 <- getPosition
         ; return $ mkConstEx "!" Nothing pos1 pos2
         }

-- More complex structures

list :: Stream s m Char => ParserT s m (SExpr PosSpan)
list = (try listEmpty) <|> (try listNonEmpty) -- <?> ("list")  
    where listEmpty = do { pos1 <- getPosition
                         ; symbol "[]"
                         ; pos2 <- getPosition
                         ; return $ mkList [] Nothing pos1 pos2 
                         }
          listNonEmpty = do { pos1 <- getPosition 
                            ; (hds, optTl) <- brackets $ do 
                                  es <- listatoms
                                  tl <- optionMaybe tail
                                  return (es, tl) 
                            ; pos2 <- getPosition
                            ; return $ mkList hds optTl pos1 pos2
                            }  
          listatoms = commaSep1 argExpr
          tail      = do { symbol "|"
                         ; variable <|> list 
                           -- Comment: This disallows irregular lists
                         }
        

lambda :: Stream s m Char => ParserT s m (SExpr PosSpan)
lambda = try ( do 
    { pos1 <- getPosition
    ; symbol "\\~" 
    ; vars <- parens $ commaSep1 $ varLit -- <?> "lambda variables"
    ; symbol "=>"
    ; ex   <- try fullExpr <|> allExpr -- TODO : clear the matter: maybe argExpr here
    ; pos2 <- getPosition
    ; return $ mkLam (vars) ex pos1 pos2
    } ) --  <?> "lambda"
    where varLit = do { pos1 <- getPosition
                      ; var  <- varIdent 
                      ; pos2 <- getPosition
                      ; return $ mkVar var pos1 pos2
                      }


-- Application tries to parse an atomic expression applied on
-- any number of sets of arguments.
-- If there are no arguments, just the atomic expression is
-- parsed.
-- This along with "lambda" and expressions with operators
-- describe all expressions.
application :: Stream s m Char => ParserT s m (SExpr PosSpan)
application  = try ( do 
    { pos1 <- getPosition
    ; s    <- atomicExpr
    ; as   <- many args -- as :: [[SExpr PosSpan]]  
    ; pos2 <- getPosition
    ; case as of 
        [] -> return s
        _  -> return $ nestedApp s as
    } -- <?> ("application")
    )
    where 
        args = do { pos1    <- getPosition
                  ; argsSet <- parens $ commaSep1 (try argExpr <|> allExpr)  
                                                --Used to say "try argExpr"
                  ; pos2    <- getPosition
                  ; return (argsSet,  pos2)
                  }

-- Everything except application or lambda. Functions as head
-- of application
atomicExpr :: Stream s m Char => ParserT s m (SExpr PosSpan)
atomicExpr = choice [ predConst
                    , list
                    , constant
                    , variable
                    , numberExpr
                    , cut
                    , parens (try fullExpr <|> allExpr)
                            -- FIXME : is allExpr really
                            -- needed here?
                    ] -- <?> ("atomicExpr")

-- Used as an argument to expr to create expr. parser
allExpr :: Stream s m Char => ParserT s m (SExpr PosSpan)
allExpr = try lambda <|> application


-- Expressions with operators, built from the operator table
-- with buildExpressionParser
-- TODO: better error messages when failing to parse operator

-- Expression for arguments (no ',' operator allowed)
argExpr :: Stream s m Char => ParserT s m (SExpr PosSpan)
argExpr = do { st <- getState
             ; let opTbl = map snd $ cachedTable st
             ; buildExpressionParser opTbl allExpr
             } -- <?> "operator"

-- General expressions (',' operator allowed)
fullExpr :: Stream s m Char => ParserT s m (SExpr PosSpan)
fullExpr = do { -- Grab state and add ',' operator
                st <- getState
              ; let opTbl = map snd cachedTable''
                        where cachedTable'  = cachedTable st
                              -- Find the right spot to put ','
                              (t1, t2) = span ((<1000).fst) cachedTable'
                              cachedTable'' = case t2 of 
                                  []        -> t1 ++ [ (1000, [commaOp])]
                                  (hd2:tl2) -> if fst hd2 == 1000 
                                                 then t1 ++ (1000, commaOp:snd hd2) : tl2
                                                 else t1 ++ (1000, [commaOp]) : t2
                              -- ',' must get special treatment because it is not a graphic token
                              commaOp = Infix (try $ do { pos1 <- getPosition
                                                        ; symbol ","
                                                        ; pos2 <- getPosition
                                                        ; return $ (\x y-> mkOpExpr (mkConst "," pos1 pos2) [x,y])
                                                        }
                                              ) AssocLeft
                -- Now build an expression parser with the new operator table
              ; buildExpressionParser opTbl allExpr
              } -- <?> "operator"


-- Head of a clause 
head_c :: Stream s m Char => ParserT s m (SExpr PosSpan)
head_c = try $ do pos1 <- getPosition
                  c    <- atom 
                  pos2 <- getPosition 
                  as   <- many args 
                  pos3 <- getPosition
                  let c' = mkConstEx c Nothing pos1 pos2
                  return $ nestedApp c' as 
    where args = do { pos1 <- getPosition
                    ; as   <- parens $ commaSep1 argExpr
                    ; pos2 <- getPosition
                    ; return (as, pos2)
                    }

-- Clause
clause :: Stream s m Char => ParserT s m (SSent PosSpan)
clause = try $ do pos1 <- getPosition
                  h <- head_c
                  b <- optionMaybe $ do 
                      gets <- symbol ":-" <|> symbol "<-" -- mono or poly?
                      body <- try fullExpr <|> allExpr
                      return (mkGets gets, body)
                  symbol "."
                  pos2 <- getPosition
                  return $ SSent_clause $ SClause (mkSpan pos1 pos2) h b


{-
 - Here follow the types of acceptable sentences.
 - Currently only working with clauses.
 - TODO : add
 -  - Goals
 -}

-- Directives
command :: Stream s m Char => ParserT s m (SSent PosSpan)
command = do { pos1 <- getPosition 
             ; symbol ":-"
             ; ex <- try fullExpr <|> allExpr
             ; symbol "."
             ; pos2 <- getPosition
             ; return $ SSent_comm $ SCommand (mkSpan pos1 pos2) ex
             }

-- Goals
goal :: Stream s m Char => ParserT s m (SSent PosSpan)
goal = do { pos1 <- getPosition
          ; symbol "?-"
          ; ex <- try fullExpr <|> allExpr
          ; symbol "."
          ; pos2 <- getPosition
          ; return $ SSent_comm $ SCommand (mkSpan pos1 pos2) ex
          }

-- Sentence
sentence :: Stream s m Char => ParserT s m (SSent PosSpan)
sentence = choice [ try goal
                  , try command
                  , try clause
                  ]  -- <?> ("sentence") 

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
          st <- getState
          updateState (\st -> st{ operatorTable = (t st)})
          cached <- buildOpTable
          updateState (\st -> st{ cachedTable = cached })
      where t st = Operators.updateOpTable (operatorTable st) op
            op   = mkOp ((fromInteger p)::Int, a, n)
            mkOp (p, a, n) = (p, Operators.Operator n a)

opDirective _ = return ()


-- Build operator table, save it in monadic state
buildOpTable :: Stream s m Char => ParsecT s (ParseState s m) m [(Int, [Operator s (ParseState s m) m (SExpr PosSpan)])]
buildOpTable = do { st <- getState
                  ; return $ mkOpTable (operatorTable st)
                  }
    where mkOpTable ops = map (\(p, op) -> (p, map opMap op)) $ Operators.groupByPrec ops
              where 
                  opMap op | Operators.isPrefixOp  op = prefixOp (Operators.opName op)
                           | Operators.isPostfixOp op = postfixOp (Operators.opName op)
                           | otherwise                = infixOp (Operators.opName op) (assocMap op)
                      where 
                            infixOp name assoc = Infix (oper (\n -> \x -> \y -> mkOpExpr n [x,y]) name) assoc
                            prefixOp name      = Prefix  (oper (\n -> \x -> mkOpExpr n [x]) name) 
                            postfixOp name     = Postfix (oper (\n -> \x -> mkOpExpr n [x]) name)
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


