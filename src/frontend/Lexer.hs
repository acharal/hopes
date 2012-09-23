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

module Lexer (lexer)
where

import Loc (Loc(..), Located, located, getLocSpan, spanEnd)
import ParseUtils (ParserT, ParserInput, Token(..), getInput, setInput, setTok, parseErrorWithLoc)
import Data.Char (isSpace, isUpper, isLower, isDigit, isAlpha)
import Pretty (sep,text,quotes,char)


lexer :: Monad m => (Located Token -> ParserT m a) -> ParserT m a
lexer cont = lexToken >>= \tok -> setTok tok >> cont tok

lexError :: Monad m => String -> Loc -> ParserT m a
lexError []  l = parseErrorWithLoc l $ text "Unexpected end of input"
lexError inp l = parseErrorWithLoc l msg
   where msg = sep [text "Unexpected character", quotes (char (head inp))]

lexToken :: Monad m => ParserT m (Located Token)

lexToken = do
    inp <- getInput
    ls  <- getLocSpan
    lexToken' inp (spanEnd ls)

lexToken' :: Monad m => ParserInput -> Loc -> ParserT m (Located Token)
lexToken' inp l = do
    case scanTok inp l of
        TokEnd -> do
            return $ located l TKEOF
        TokError l2 inp2 -> do
            lexError inp2 l2
        TokSkip l2 inp2 -> do
            lexToken' inp2 l2
        Tok t len inp2 -> do
            let l2 = l{locOffset=(locOffset l)+len}
            setInput inp2
            return $ located (l,l2) t

data ScanResult =
    Tok Token Int String
  | TokEnd
  | TokSkip  Loc String
  | TokError Loc String

type ScanAction = String -> Loc -> ScanResult

scanTok :: ScanAction
scanTok [] loc               = TokEnd
scanTok inp@('\n':cs) loc    = scanSkip inp loc
scanTok inp@('%':cs) loc     = scanSkipLine inp loc
scanTok ('(':cs) loc         = Tok TKoparen 1 cs
scanTok (')':cs) loc         = Tok TKcparen 1 cs
scanTok ('[':cs) loc         = Tok TKobrak  1 cs
scanTok (']':cs) loc         = Tok TKcbrak  1 cs
scanTok ('{':cs) loc         = Tok TKocurly 1 cs
scanTok ('}':cs) loc         = Tok TKccurly 1 cs
scanTok ('|':cs) loc         = Tok TKvert   1 cs
scanTok ('/':cs) loc         = Tok TKslash  1 cs
scanTok ('\\':cs) loc        = Tok TKbslash 1 cs
scanTok (',':cs) loc         = Tok TKcomma  1 cs
scanTok ('.':cs) loc         = Tok TKdot    1 cs
scanTok ('_':cs) loc         = Tok TKwild   1 cs
scanTok ('!':cs) loc         = Tok TKcut    1 cs
scanTok (';':cs) loc         = Tok TKsemi   1 cs
scanTok ('\'':cs) loc        = Tok TKsq     1 cs
scanTok ('=':cs) loc         = Tok TKeq     1 cs
scanTok (':':'-':cs) loc     = Tok TKgets   2 cs
scanTok (':':':':cs) loc     = Tok TKcolcol 2 cs
scanTok ('-':'>':cs) loc     = Tok TKarrow  2 cs
scanTok inp@(c:cs) l
    | isSpace c              = scanSkip inp l
    | isUpper c              = scanName TKid inp l
    | isLower c || isDigit c = scanName TKid inp l
scanTok inp loc              = TokError loc inp


scanName :: (String -> Token) -> ScanAction
scanName cstr cs l   = Tok (cstr name) (length name) rest
   where (name,rest) = span isNameChar cs
         isNameChar c = isAlpha c || isDigit c || (c == '_')

scanSkip :: ScanAction
scanSkip inp lo =
    let scanSkip_aux o l (c:cs) lo
                 | c == '\n'              = scanSkip_aux 1 (l+1) cs lo
                 | isSpace c              = scanSkip_aux (o+1) l cs lo
                 | otherwise              = TokSkip lo{locOffset=o, locLine=l} (c:cs)
        scanSkip_aux o l [] lo            = TokEnd
    in scanSkip_aux (locOffset lo) (locLine lo) inp lo

scanSkipLine :: ScanAction
scanSkipLine inp lo = scanSkip inp2 lo{locOffset=1, locLine=(locLine lo) + 1}
    where (com, ('\n':inp2)) = span (/='\n') inp

