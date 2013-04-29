module Language.Prolog.Lexer (
        module Text.Parsec.Token,
        prologStyle, prologDef, prolog, stringLiteral2, varIdent, conIdent
    ) where

import Text.Parsec
import Text.Parsec.Token
import qualified Text.Parsec.Token as L
import Text.Parsec.Language

type Lexer s u m = GenTokenParser s u m

prologStyle :: Stream s m Char => GenLanguageDef s u m
prologStyle = emptyDef 
               { commentStart    = "/*"
               , commentEnd      = "*/"
               , commentLine     = "%"
               , nestedComments  = False
               , identStart      = letter <|> digit <|> oneOf "#$&*+-./:<=>?@^~\\_"
               , identLetter     = alphaNum <|> oneOf "_"
               , opStart         = identStart prologStyle
               , opLetter        = identLetter prologStyle
               , reservedNames   = []
               , reservedOpNames = []
               , caseSensitive   = True
               }


prologDef :: Stream s m Char => GenLanguageDef s u m
prologDef = prologStyle

prolog :: Stream s m Char => GenTokenParser s u m
prolog  = makeTokenParser prologDef

varIdent :: Stream s m Char => GenTokenParser s u m -> ParsecT s u m String
varIdent lexer = lexeme lexer $ try $
    do { c  <- underscore <|> upper
       ; cs <- many (underscore <|> alphaNum) 
       ; return (c:cs)
       }
    where underscore = char '_'

conIdent :: Stream s m Char => GenTokenParser s u m -> ParsecT s u m String
conIdent lexer = lexeme lexer $ try $ 
          choice [ graphicTokenString
                 , letterIdent 
                 ]
    where graphicTokenString = do { c <- graphicToken
                                  ; cs <- many (graphicToken <|> identLetter)
                                  ; return (c:cs)
                                  }
          letterIdent  = do { c  <- identStart
                            ; cs <- many identLetter
                            ; return (c:cs)
                            }
          identStart   = lower
          identLetter  = alphaNum <|> underscore
          graphicToken = oneOf "#$&*+-./:<=>?@^~\\"
          underscore   = char '_'

stringLiteral2 :: Stream s m Char => GenTokenParser s u m -> ParsecT s u m String
stringLiteral2 lexer = quotedString lexer '\'' <|> quotedString lexer '"'

quotedString :: Stream s m Char => GenTokenParser s u m -> Char -> ParsecT s u m String
quotedString lexer quote = lexeme lexer (
                   do{ str <- between (char quote)
                                      (char quote <?> "end of string")
                                      (many (stringChar quote))
                     ; return (foldr (maybe id (:)) "" str)
                     }
                   <?> "literal string")

stringChar :: Stream s m Char => Char -> ParsecT s u m (Maybe Char)
stringChar quote = do{ c <- stringLetter quote; return (Just c) }
             <|> stringEscape
             <|> escapedQuote quote

stringLetter :: Stream s m Char => Char -> ParsecT s u m Char
stringLetter quote = satisfy (\c -> (c /= quote) && (c /= '\\'))

stringEscape :: Stream s m Char => ParsecT s u m (Maybe Char)
stringEscape = do { char '\\'
                  ;     do { escapeGap ; return Nothing }
                    <|> do { esc <- escapeCode; return (Just esc) }
                  }

escapedQuote :: Stream s m Char => Char -> ParsecT s u m (Maybe Char)
escapedQuote quote = try $ do { char quote; c <- char quote; return (Just c) }

escapeGap :: Stream s m Char => ParsecT s u m Char
escapeGap = do { many1 space
               ; char '\\' <?> "end of string gap"
               }

escapeCode :: Stream s m Char => ParsecT s u m Char
escapeCode = charEsc

charEsc :: Stream s m Char => ParsecT s u m Char
charEsc = choice (map parseEsc escMap)
    where parseEsc (c,code) = do{ char c; return code }
          escMap = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")

