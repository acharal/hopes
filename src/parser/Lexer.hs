module Lexer where

import Loc
import ParseUtils
import Char



isNameChar c = isAlpha c || isDigit c || (c == '_')
isVar str = isUpper (head str)

lexError :: String -> Parser a
lexError inp = fail ("Unrecognized character")

lexer :: (Located Token -> Parser a) -> Parser a
lexer cont = lexToken >>= \tok -> setLastTok tok >> cont tok

lexToken :: Parser (Located Token)
lexToken = do
    inp <- getSrcBuf
    loc <- getSrcLoc
    case scanTok inp loc of
        TokEnd -> do
            return (mkLTk loc loc TKEOF)
        TokError loc2 inp2 -> do
            lexError inp2
        TokSkip loc2 inp2 -> do
            setSrcBuf inp2
            setSrcLoc loc2
            lexToken
        Tok t len inp2 -> do
            let loc2 = loc{locOffset=(locOffset loc)+len}
            setSrcBuf inp2
            setSrcLoc loc2
            return (mkLTk loc loc2 t)

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
scanTok (':':'-':cs) loc     = Tok TKgets   2 cs
scanTok (':':':':cs) loc     = Tok TKcolcol 2 cs
scanTok ('-':'>':cs) loc     = Tok TKarrow  2 cs
scanTok inp@(c:cs) loc
    | isSpace c              = scanSkip inp loc
    | isUpper c              = scanName TKid inp loc
    | isLower c || isDigit c = scanName TKid inp loc
scanTok inp loc              = TokError loc inp

scanName :: (String -> Token) -> ScanAction
scanName cstr cs loc = Tok (cstr name) (length name) rest
   where (name,rest) = span isNameChar cs

scanSkip :: ScanAction
scanSkip inp loc = 
    let scanSkip_aux o l (c:cs) loc
                 | c == '\n'              = scanSkip_aux 1 (l+1) cs loc
                 | isSpace c              = scanSkip_aux (o+1) l cs loc
                 | otherwise              = TokSkip loc{locOffset=o, locLine=l} (c:cs)
        scanSkip_aux o l [] loc           = TokEnd
    in scanSkip_aux (locOffset loc) (locLine loc) inp loc

scanSkipLine :: ScanAction
scanSkipLine inp loc = scanSkip inp2 loc{locOffset=1, locLine=(locLine loc) + 1}
    where (com, ('\n':inp2)) = span (/='\n') inp

