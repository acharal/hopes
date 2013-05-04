
module Parser where

import Loc
--import ParseUtils
import qualified Lexer as L
import Syntax
import qualified Operator as Operators
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
            , cachedTable   :: [(Int, [Operator s (ParseState s m) m ( SExpr () )])]
            } 

type ParserT s m a = ParsecT s (ParseState s m) m a


-- helpers

followedBy p after = do { rv <- p; after; return rv }

-- |  opposite of take
skip n [] = []
skip 0 x = x
skip n (x:xs) = skip (n-1) xs

-- Make expressions
mkConst :: String -> SExpr ()
mkConst s   = SExpr_const () (Const () s) False Nothing Nothing

mkPredCon :: String -> Maybe Integer -> SExpr ()
mkPredCon s i = SExpr_predCon () (Const () s) i Nothing

mkVar :: String -> Var ()
mkVar s = if s=="_" then AnonVar () else Var () s

mkVarEx :: String -> SExpr ()
mkVarEx s = SExpr_var () $ mkVar s

mkList :: [SExpr ()] -> Maybe (SExpr ())-> SExpr ()
mkList hds tl = SExpr_list () hds tl

mkLam :: [Var ()] -> SExpr () -> SExpr ()
mkLam vars bd = SExpr_lam () vars bd

mkOp :: String -> [SExpr ()] -> SExpr ()
mkOp opName args = SExpr_op () (Const () opName) args

mkGets :: String -> SGets
mkGets "<-" = SGets_poly
mkGets ":-" = SGets_mono
mkGets _    = error "gets" -- TODO some better error discipline

mkHead :: String -> [[ SExpr () ]] -> SHead ()
mkHead c args = SHead () (Const () c) args

mkClause :: SHead () -> Maybe (SGets, SExpr ()) -> SClause ()
mkClause h b = SClause () h b


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

-- Constant or graphic operator
conIdent :: Stream s m Char => ParsecT s u m String
conIdent = L.conIdent L.prolog

natural :: Stream s m Char => ParsecT s u m Integer
natural = L.natural L.prolog

atom :: Stream s m Char => ParsecT s u m String
atom = try conIdent <|> stringLiteral <?> "atom"

naturalOrFloat :: Stream s m Char => ParsecT s u m (Either Integer Double)
naturalOrFloat = L.naturalOrFloat L.prolog

integer :: Stream s m Char => ParsecT s u m Integer
integer = L.integer L.prolog

float :: Stream s m Char => ParsecT s u m Double
float = L.float L.prolog

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = L.symbol L.prolog


--reserved
predTK :: Stream s m Char => ParsecT s u m ()
predTK = L.reserved L.prolog "pred"

trueTK :: Stream s m Char => ParsecT s u m ()
trueTK = L.reserved L.prolog "true"

failTK :: Stream s m Char => ParsecT s u m ()
failTK = L.reserved L.prolog "fail"




-- parser 
-- TODO : Add location information
-- TODO : implement optional arities in constants/ 
--        polymorphic definitions
-- TODO : implement type annotations

-- Basic structures

variable :: Stream s m Char => ParserT s m (SExpr ())
variable = try ( do { s  <- varIdent
                    ; return $ mkVarEx s
                    } <?> ("variable")
               )

constant :: Stream s m Char => ParserT s m (SExpr ())
constant = try ( do { c <- atom
                    ; return $ mkConst c
                    } <?> ("constant")
               )

predConst :: Stream s m Char => ParserT s m (SExpr ())
predConst = try ( do  { predTK
                      ; s <- atom
                      ; n <- optionMaybe $ do 
                             char '/'
                             n' <- natural
                             return n'
                      ; return $ mkPredCon s n
                      } <?> ("predicate constant")
                )

natExpr :: Stream s m Char => ParserT s m (SExpr ())
natExpr = try ( do { n <- natural
                   ; return $ SExpr_int () n
                   } <?> ("integer constant")
              )

floatExpr :: Stream s m Char => ParserT s m (SExpr ())
floatExpr = try ( do { f <- float
                     ; return $ SExpr_float () f
                     } <?> ("float constant")
                )

{- Why implement these separately? 
trueExpr :: Stream s m Char => ParserT s m (SExpr ())
trueExpr = try $ do { e <- trueTK
                    ; return sTrue
                    }

failExpr :: Stream s m Char => ParserT s m (SExpr ())
failExpr = try $ do { e <- failTK
                    ; return sFail
                    }
-}
{-
number :: Stream s m Char => ParserT s m Expr
number = try $  do { num <- try naturalOrFloat <|> eitherInteger
                   ; return (Num num)
                   }
    where eitherInteger = do { i <- integer; return (Left i); }        
-}

cut :: Stream s m Char => ParserT s m (SExpr ())
cut = do { L.symbol L.prolog "!"; return sCut}


list :: Stream s m Char => ParserT s m (SExpr ())
list = try listEmpty <|> listNonEmpty <?> ("list")  
    where 
        listEmpty = do { symbol "[]"; return sNil }
        listNonEmpty = brackets $ do 
                         es <- listatom
                         tl <- optionMaybe tail
                         return $ mkList es tl 
        listatom  = commaSep1 argExpr
        tail      = do { symbol "|"
                       ; variable <|> list 
                         -- Comment: This disallows 
                         -- irregular lists
                       }
        

-- More complex structures

lambda :: Stream s m Char => ParserT s m (SExpr ())
lambda = try ( do 
    { symbol "\\~" 
    ; vars <- (parens $ commaSep1 $ varIdent) <?> "lambda variables"
    ; symbol "=>"
    ; ex <- fullExpr 
    ; return $ mkLam (map mkVar vars) ex
    } ) <?> "lambda"


application :: Stream s m Char => ParserT s m (SExpr ())
application  = try ( do 
    { s  <- atomicExpr
    ; as <- many args -- as :: [[SExpr ()]]  
    ; case as of 
        [] -> return s
        _  -> return $ nested_app s as
    } <?> ("application")
    )
    where -- args = try (parens expr) <|> parens (do { e <- commaSep1 expr; return (concatMap id e)} )
        args = parens $ commaSep1 (try argExpr)
        -- From a list of argument lists and a head, make a
        -- nested application
        nested_app s as = {- :: SExpr -> [[SExpr]] -> SExpr -} 
            foldl (SExpr_app () ) s as

-- Everything except application or lambda. Functions as head
-- of application
atomicExpr :: Stream s m Char => ParserT s m (SExpr ())
atomicExpr = choice [ variable
                    , constant
                    , predConst
                    , natExpr
                    , floatExpr
                    --, trueExpr
                    --, failExpr
                    , list
                    , cut
                    , parens (try fullExpr <|> allExpr)
                            -- FIXME : is allExpr really
                            -- needed here?
                    ] <?> ("atomicExpr")

-- Everything
allExpr :: Stream s m Char => ParserT s m (SExpr ())
allExpr = try lambda <|> application

{-
term :: Stream s m Char => ParserT s m (SExpr ())
term = choice [ variable
              , struct
              , number
              , constant
              , list
              , cut
              , parens (try fullexpr <|> term)
              ] <?> ("term")
-}


-- Expressions with operators, built from the operator table
-- with buildExpressionParser
expr :: Stream s m Char => Int -> ParserT s m (SExpr ())
expr prec = getParser prec allExpr --TODO check this
    where getParser p t = do
                st <- getState;
                let opTbl = map snd $ takeWhile (\(p',_) -> p >= p') $ cachedTable st
                buildExpressionParser opTbl t

-- Expression for args (precedence > ',')
argExpr :: Stream s m Char => ParserT s m (SExpr ())
argExpr = expr 999

-- General expressions (any precedense)
fullExpr :: Stream s m Char => ParserT s m (SExpr ())
fullExpr = expr 1200

-- Head of a clause
head_c :: Stream s m Char => ParserT s m (SHead ())
head_c = try $ do c    <- atom  
                  args <- many $ parens $ commaSep1 argExpr
                  return $ mkHead c args

-- Clause
clause :: Stream s m Char => ParserT s m (SSent ())
clause = try $ do h <- head_c
                  b <- optionMaybe $ do 
                      gets <- symbol ":-" <|> symbol "<-" -- mono or poly?
                      body <- fullExpr
                      return (mkGets gets, body)
                  return $ SSent_clause () $ SClause () h b


{-
 - Here follow the types of acceptable sentences.
 - Currently only working with clauses.
 - TODO : add
 -  - Commands
 -  - Goals
 -}


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
-}

-- Directives
command :: Stream s m Char => ParserT s m (SSent ())
command = do { ex <- between inf dot fullExpr
             ; return $ SSent_comm () $ SCommand () ex
             }


{-
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



--sentence :: Stream s m Char => ParserT s m (SExpr ())
--sentence = fullExpr `followedBy` dot <?> ("sentence") 

-- TODO : Add more sentence types
sentence :: Stream s m Char => ParserT s m (SSent ())
sentence = try (clause `followedBy` dot) <|> command <?> ("sentence") 

isCommand (SSent_comm _ _ ) = True
isCommand _ = False

-- | Directives must be only in goal clause (without head literal)
-- Only recognizing op directives thus far TODO add more
opDirective1 (SSent_comm _ (SCommand _ e)) = opDirective e
opDirective1 _ = return ()

opDirective ( SExpr_app _ (SExpr_const _ (Const _ "op") _ _ _ ) 
                            -- TODO improve the last args to None (Just 3)) 
            [ SExpr_int _ p
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
buildOpTable :: Stream s m Char => ParsecT s (ParseState s m) m [(Int, [Operator s (ParseState s m) m (SExpr ())])]
buildOpTable = do { st <- getState
                  ; return $ mkOpTable (operatorTable st)
                  }
    where mkOpTable ops = map (\(p, op) -> (p, map opMap op)) $ Operators.groupByPrec ops
              where 
                  opMap op | Operators.isPrefixOp  op = prefixOp (Operators.opName op)
                           | Operators.isPostfixOp op = postfixOp (Operators.opName op)
                           | otherwise                = infixOp (Operators.opName op) (assocMap op)
                      where 
                            infixOp name assoc = Infix (oper (\n -> \x -> \y -> mkOp n [x,y]) name) assoc
                            prefixOp name      = Prefix  (oper (\n -> \x -> mkOp n [x]) name) 
                            postfixOp name     = Postfix (oper (\n -> \x -> mkOp n [x]) name)
                            assocMap op | Operators.isAssocLeft  op = AssocLeft
                                        | Operators.isAssocRight op = AssocRight
                                        | otherwise                 = AssocNone

                  oper f name = try $ do -- Buggy!
                                         --{ L.symbol L.prolog name
                                         --; notFollowedBy (choice ops2)
                                         --; return (f name) }

                                         -- Instead match the exact string and see if 
                                         -- there is an operator symbol following
                                         { string name
                                         ; notFollowedBy L.graphicToken
                                         ; L.whiteSpace L.prolog
                                         ; return $ f name
                                         } 
                    -- Not needed anymore
                    -- where ops2    = map (\x -> try (string x)) $ map (skip (length name)) opNames
                    --        opNames = filter (f2 name) $ map (Operators.opName.snd) ops
                    --        f2 n m  = length n < length m && n `isPrefixOf` m




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
    ops <- readFile "../../pl/op.pl"
    (p, optable) <- parse st' "pl/op.pl" ops
    f <- readFile input
    result <- parse optable input f
    return result
    where st' = ParseSt initOps []
          initOps = [(1200, Operators.Operator ":-" "xfx")
                    ,(1200, Operators.Operator ":-" "fx")
                    ,(1000, Operators.Operator "," "xfx")
                    ]
          parse st src input = case runPrologParser (buildTable >> many1 sentence') st src input of
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
         
parseProlog p input = 
    case runPrologParser p st "" input of 
        Left err -> do putStr "parse error at" 
                       print err
        Right (x,_) -> print x
    where st = ParseSt []

test () = do { (a,b) <- parseProlog2 "../../pl/examples/test.pl" ; print a }
