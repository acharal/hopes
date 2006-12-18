module Lexer where

import Loc
import ParseUtils
import Char

data Token =
      TKoparen
    | TKcparen
    | TKgets
    | TKdot
    | TKcomma
    | TKvert
    | TKobrak
    | TKcbrak
    | TKid String
    | TKEOF
   deriving (Eq,Show)

isNameChar c = isAlpha c || isDigit c || (c == '_')
isVar str = isUpper (head str)

lexError :: Loc -> String -> Parser a
lexError loc inp = setPLoc loc >> failP ("unrecognized character near "++show (take 5 inp))

lexer :: (Located Token -> Parser a) -> Parser a
lexer cont = lexToken >>= \tok -> cont tok

lexToken :: Parser (Located Token)
lexToken = do
    inp <- getPInput
    loc <- getPLoc
    case scanTok inp loc of 
        TokEnd -> do
            return (L (LocSpan loc loc) TKEOF)
        TokError loc2 inp2 -> do
            lexError loc2 inp2
        TokSkip loc2 inp2 -> do
            setPInput inp2
            setPLoc loc2
            lexToken
        Tok t len inp2 -> do
            let loc2 = loc{locOffset=(locOffset loc)+len}
            setPInput inp2
            setPLoc loc2
            return (L (LocSpan loc loc2) t)

data ScanResult = 
    Tok Token Int String
  | TokEnd 
  | TokSkip  Loc String
  | TokError Loc String

type ScanAction = String -> Loc -> ScanResult

scanTok :: ScanAction
scanTok [] loc               = TokEnd
scanTok inp@('\n':cs) loc    = scanSkip inp loc
scanTok ('(':cs) loc         = Tok TKoparen 1 cs
scanTok (')':cs) loc         = Tok TKcparen 1 cs
scanTok ('[':cs) loc         = Tok TKobrak  1 cs
scanTok (']':cs) loc         = Tok TKcbrak  1 cs
scanTok ('|':cs) loc         = Tok TKvert   1 cs
scanTok (',':cs) loc         = Tok TKcomma  1 cs
scanTok ('.':cs) loc         = Tok TKdot    1 cs
scanTok (':':'-':cs) loc     = Tok TKgets   2 cs
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
                 | isSpace c              = scanSkip_aux (o+1) l cs loc
                 | c == '\n'              = scanSkip_aux 0 (l+1) cs loc
                 | otherwise              = TokSkip loc{locOffset=o, locLine=l} (c:cs)
        scanSkip_aux o l [] loc           = TokEnd
    in scanSkip_aux (locOffset loc) (locLine loc) inp loc

testlexer str = 
    let getTokens =
            lexToken >>= \t -> 
            let L _ t' = t in
            if t' == TKEOF then 
                return []
            else getTokens >>= \ts ->  return (t:ts)

        p = setPInput str >> getTokens
        initst :: ParseState
        initst = PState str (Loc "" 1 0) (Loc "" 1 0)
    in case (runP p) initst of 
          POk _ ts -> ts
          PFailed l err -> error (show err)
