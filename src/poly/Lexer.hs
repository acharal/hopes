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


-- The module implementing a lexer for polyHOPES

module Lexer (
        module Text.Parsec.Token,
        hopesStyle, hopesDef, hopes, stringLiteral2, varIdent, conIdent,graphicToken
    ) where

import Text.Parsec
import Text.Parsec.Token
import qualified Text.Parsec.Token as L
import Text.Parsec.Language

type Lexer s u m = GenTokenParser s u m

-- A general language definition for a hopes-like higher order language
hopesStyle :: Stream s m Char => GenLanguageDef s u m
hopesStyle = emptyDef 
               { commentStart    = "/*"
               , commentEnd      = "*/"
               , commentLine     = "%"
               , nestedComments  = False
               , identStart      = letter <|> digit <|> graphicToken
               , identLetter     = alphaNum <|> oneOf "_"
               , opStart         = identStart hopesStyle
               , opLetter        = identLetter hopesStyle
               , reservedNames   = ["pred", "true", "false"]
               , reservedOpNames = ["=>", "\\~"]
               , caseSensitive   = True
               }

graphicToken :: Stream s m Char => ParsecT s u m Char
graphicToken = oneOf "#$&*+-;/:<=>?@^~\\|"

hopesDef :: Stream s m Char => GenLanguageDef s u m
hopesDef = hopesStyle

-- The actual polyHOPES lexer
hopes :: Stream s m Char => GenTokenParser s u m
hopes  = makeTokenParser hopesDef

-- Variable token ( _ or alphanumeric starting with capital)
varIdent :: Stream s m Char => GenTokenParser s u m -> ParsecT s u m String
varIdent lexer = lexeme lexer $ try $
    do { c  <- underscore <|> upper
       ; cs <- many (underscore <|> alphaNum) 
       ; return (c:cs)
       }
    where underscore = char '_'

-- Constant. Either alphanumeric starting with small letter, or graphic token string
conIdent :: Stream s m Char => GenTokenParser s u m -> ParsecT s u m String
conIdent lexer = lexeme lexer $ try $ 
          choice [ graphicTokenString
                 , letterIdent 
                 ]
    where graphicTokenString = do { tk  <- graphicToken
                                  ; tks <- many (graphicToken <|> char '.')
                                  ; return (tk:tks)
                                  }
          letterIdent  = do { c  <- identStart
                            ; cs <- many identLetter
                            ; return (c:cs)
                            } 
          identStart   = lower
          identLetter  = alphaNum <|> underscore
          underscore   = char '_'

-- atom surrounded by " or '. TODO " -> char list
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


-- Escape characters
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

