{
module Parser where

import Loc
import ErrUtils
import ParseUtils
import Lexer
import HOPL
import IO
}

%token 
      ':-'      { (L _ TKgets) }
      '.'       { (L _ TKdot)  }
      ','       { (L _ TKcomma) }
      '('       { (L _ TKoparen) }
      ')'       { (L _ TKcparen) }
      ID        { (L _ (TKid   $$)) }

%monad { Parser } { >>= } { return }
%lexer { lexer } { (L _ TKEOF) }
%name parseProg  prog
%name parseGoal  goal
%name parseTerm  term
%name parseTerms terms
%name parse  prog
%tokentype { (Located Token) }
%% 

prog :: { HoProg }
     : clauses                { reverse $1 }

clauses :: { [HoClause] }
        : clause            { [$1] }
        | clauses clause    { $2:$1 }

clause :: { HoClause }
       : rule               { $1 }
       | fact               { $1 }

fact :: { HoClause }
     : atom '.'             { ($1, []) }

rule :: { HoClause }
     : atom ':-' atoms '.'  { ($1, reverse $3) }

atom :: { HoAtom }
     : ID '(' terms ')'     { HoAtom $1 (reverse $3) }

atoms  :: { [HoAtom] }
       : atoms ',' atom     { $3:$1 }
       | atom               { [$1] }

term :: { HoTerm }
     : ID                    { if isVar $1 then HoVar $1 else HoConst $1 }
     | ID '(' terms ')'      { HoFun $1 (reverse $3) }

terms  :: { [HoTerm] }
       : terms ',' term     { $3:$1 }
       | term               { [$1] }

goal :: { HoGoal }
     : atoms                { $1 }

{


happyError :: Parser a
happyError = P $ \PState{buffer=buf, loc=loc} -> PFailed loc ( (show loc) ++ ": Parse error near " ++ (show (take 10 buf)))


testParse str = 
    case runP parse (mkState str) of
        POk _ prog -> do
                putStr (show prog)
        PFailed _ err -> do
                printError err
}
