
module Language.Prolog.Parser where

import qualified Language.Prolog.Lexer as L
import Language.Prolog.Syntax
import qualified Language.Prolog.Operator as Operators
import Text.Parsec
import Text.Parsec.Expr
import Data.Char
import Data.List
import Control.Monad (when)

import qualified Data.ByteString.Char8 as C
import Text.Parsec.ByteString

-- type OperatorTable s u m a = [[Operator s u m a]]

data ParseState s m = 
    ParseSt { operatorTable :: Operators.OperatorTable
            , cachedTable   :: [(Int, [Operator s (ParseState s m) m Expr])]
            } 

type ParserT s m a = ParsecT s (ParseState s m) m a


-- helpers

followedBy p after = do { rv <- p; after; return rv }

-- |  opposite of take
skip n [] = []
skip 0 x = x
skip n (x:xs) = skip (n-1) xs

-- lexer 

commaSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep  = L.commaSep1 L.prolog

commaSep1 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep1 = L.commaSep1 L.prolog

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = L.parens L.prolog

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme = L.lexeme L.prolog

identifier :: Stream s m Char => ParsecT s u m String
identifier = L.identifier L.prolog

dot :: Stream s m Char => ParsecT s u m String
dot = L.dot L.prolog

inf :: Stream s m Char => ParsecT s u m String
inf = L.symbol L.prolog ":-"

stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = L.stringLiteral2 L.prolog

brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = L.brackets L.prolog

varIdent :: Stream s m Char => ParsecT s u m String
varIdent = L.varIdent L.prolog

conIdent :: Stream s m Char => ParsecT s u m String
conIdent = L.conIdent L.prolog

naturalOrFloat :: Stream s m Char => ParsecT s u m (Either Integer Double)
naturalOrFloat = L.naturalOrFloat L.prolog

integer :: Stream s m Char => ParsecT s u m Integer
integer = L.integer L.prolog

-- parser

variable :: Stream s m Char => ParserT s m Expr
variable =  do { s <- varIdent
               ; return (Var s)
               } <?> ("variable")

struct :: Stream s m Char => ParserT s m Expr
struct =  do { s  <- atom
             ; as <- option [] args
             ; return (Str s as)
             } <?> ("structure")
    where -- args = try (parens expr) <|> parens (do { e <- commaSep1 expr; return (concatMap id e)} )
          args = parens $ commaSep1 (try argexpr <|> term)
          atom = conIdent <|> stringLiteral

constant :: Stream s m Char => ParserT s m Expr
constant =
       do { s <- lexeme (many1 alphaNum)
          ; return (Str s [])
          }

number :: Stream s m Char => ParserT s m Expr
number = try $  do { num <- try naturalOrFloat <|> eitherInteger
                   ; return (Num num)
                   }
    where eitherInteger = do { i <- integer; return (Left i); }        

term :: Stream s m Char => ParserT s m Expr
term = choice [ variable
              , struct
              , number
              , constant
              , list
              , cut
              , parens (try fullexpr <|> term)
              ] <?> ("term")

cut :: Stream s m Char => ParserT s m Expr
cut = do { L.symbol L.prolog "!"; return (Str "!" []) }

list :: Stream s m Char => ParserT s m Expr
list = try listEmpty <|> listNonEmpty <?> ("list")  
    where 
        listatom  = commaSep1 argexpr
        tail      = do { slash
                       ; variable <|> listEmpty
                       }
        slash     = L.symbol L.prolog "|"
        listEmpty = do { L.symbol L.prolog "[]"; return (Str "[]" []); }
        listNonEmpty = brackets $ do { es <- listatom
                                     ; tl <- option (Str "[]" []) tail
                                     ; return $ foldr (\n -> \m -> Cons n m) tl es 
                                     }

argexpr :: Stream s m Char => ParserT s m Expr
argexpr = expr 999

fullexpr :: Stream s m Char => ParserT s m Expr
fullexpr = expr 1200

expr :: Stream s m Char => Int -> ParserT s m Expr
expr prec = getParser prec term
    where getParser p t = do
                st <- getState;
                let opTbl = map snd $ takeWhile (\(p',_) -> p >= p') $ cachedTable st
                buildExpressionParser opTbl t

buildOpTable :: Stream s m Char => ParsecT s (ParseState s m) m [(Int, [Operator s (ParseState s m) m Expr])]
buildOpTable = do { st <- getState
                  ; return $ mkOpTable (operatorTable st)
                  }
    where mkOpTable ops = map (\(p, op) -> (p, map opMap op)) $ Operators.groupByPrec ops
            where 
              oper f name = try $ do { L.symbol L.prolog name
                                     ; notFollowedBy (choice ops2)
                                     ; return (f name) }
                   where ops2    = map (\x -> try (string x)) $ map (skip (length name)) opNames
                         opNames = filter (f2 name) $ map (Operators.opName.snd) ops
                         f2 n m  = length n < length m && n `isPrefixOf` m

              opMap op | Operators.isPrefixOp  op = prefixOp (Operators.opName op)
                       | Operators.isPostfixOp op = postfixOp (Operators.opName op)
                       | otherwise                = infixOp (Operators.opName op) (assocMap op)
                       where 
                             infixOp name assoc = Infix   (oper (\n -> \x -> \y -> Op n [x,y]) name) assoc

                             prefixOp name      = Prefix  (oper (\n -> \x -> Op n [x]) name)
 
                             postfixOp name     = Postfix (oper (\n -> \x -> Op n [x]) name)

                             assocMap op | Operators.isAssocLeft  op = AssocLeft
                                         | Operators.isAssocRight op = AssocRight
                                         | otherwise                = AssocNone

{-
literal :: Stream s m Char => ParserT s m Expr
literal = fullexpr <?> ("literal")

body :: Stream s m Char => ParserT s m Expr
body = fullexpr <?> ("body literal")

fact :: Stream s m Char => ParserT s m (Maybe Expr, [Expr])
fact = do {  h <- struct `followedBy` dot; return (Just h, []); }

rule :: Stream s m Char => ParserT s m (Maybe Expr, [Expr])
rule = do 
   h <- struct
--   b <- between inf dot body
   b <- (option [] (do { inf; body })) `followedBy` dot
   return (Just h, b)

clause :: Stream s m Char => ParserT s m (Maybe Expr, [Expr])
clause = rule --try fact <|> rule

command :: Stream s m Char => ParserT s m (Maybe Expr, [Expr])
command = do { b <- between inf dot body
             ; return (Nothing, b)
             }

goal :: Stream s m Char => ParserT s m (Maybe Expr, [Expr])
goal = do { b <- between q dot body
          ; return (Nothing, b)
          }
   where q = L.symbol L.prolog "?-"

sentence :: Stream s m Char => ParserT s m (Maybe Expr, [Expr])
sentence = choice [ command'
                  , goal
                  , clause
                  ] <?> ("sentence")
    where command' = do { c <- command
                        ; opDirective1 c
                       ; return c }
-}

sentence :: Stream s m Char => ParserT s m Expr
sentence = fullexpr `followedBy` dot <?> ("sentence") 

isCommand (Op ":-" [exp]) = True
isCommand _ = False

-- | Directives must be only in goal clause (without head literal)

opDirective1 (Op ":-" [e]) = opDirective e
opDirective1 _ = return ()

opDirective (Str "op" [Num (Left p), (Str a []), (Str n [])]) = do
          st <- getState
          updateState (\st -> st{ operatorTable = (t st)})
          cached <- buildOpTable
          updateState (\st -> st{ cachedTable = cached })
    where t st = Operators.updateOpTable (operatorTable st) op
          op  = mkOp ((fromInteger p)::Int, a, n)
          mkOp (p, a, n) = (p, Operators.Operator n a)

opDirective _ = return ()

-- top-level parsing
-- TODO: Parse operators in head
-- TODO: Parse predicates of form module:predicate.
-- TODO: Attach position to AST
-- TODO: Parse module directives
-- TODO: Parse include Directives

runPrologParser p st sourcename input = runP p' st sourcename input
    where p' = do 
            L.whiteSpace L.prolog
            res <- p
            st' <- getState
            return (res, st')

parseProlog2 input = do
    ops <- readFile "/home/angel/edu/phd/code/parsec-prolog/pl/op.pl"
    (p, optable)<- parse st' "pl/op.pl" ops
    f <- readFile input
    result <- parse optable input f
    return result
    where parse st src input = case runPrologParser (buildTable >> many1 sentence') st src input of
                                   Left err ->  do putStr "parse error at"
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
          ops = [(1200, Operators.Operator ":-" "xfx")
                ,(1200, Operators.Operator ":-" "fx")
                ,(1000, Operators.Operator "," "xfx")
                ]
          st' = ParseSt ops []

parseProlog p input = 
    case runPrologParser p st "" input of 
        Left err -> do putStr "parse error at" 
                       print err
        Right (x,_) -> print x
    where st = ParseSt []


