{
module Parser where

import Loc
import ErrUtils
import ParseUtils
import Lexer
import HOPL
import IO
import Data.Char
import Control.Monad.Error
}

%token 
      ':-'      { (L _ TKgets)   }
      '.'       { (L _ TKdot)    }
      ','       { (L _ TKcomma)  }
      '('       { (L _ TKoparen) }
      ')'       { (L _ TKcparen) }
      '['       { (L _ TKobrak ) }
      '{'       { (L _ TKocurly) }
      '}'       { (L _ TKccurly) }
      ']'       { (L _ TKcbrak ) }
      '{'       { (L _ TKocurly) }
      '}'       { (L _ TKccurly) }
      '|'       { (L _ TKvert )  }
      '_'       { (L _ TKwild )  }
      ID        { (L _ (TKid   $$)) }

%monad { Parser } { >>= } { return }
%lexer { lexer } { (L _ TKEOF) }
%name parseProg  prog
%name parseGoal  goal
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
     : ID                   { HoAtom $1 [] }
     | ID '(' terms ')'     { HoAtom $1 (reverse $3) }

atoms  :: { [HoAtom] }
       : atoms ',' atom     { $3:$1 }
       | atom               { [$1] }

term :: { HoTerm }
     : ID                    { if isVar $1 then HoVar $1 else if (all isDigit $1) then homkNum ((read $1)::Int) else HoConst $1 }
     | '_'                   { HoVar "_" }
     | ID '(' terms ')'      { HoFun $1 (reverse $3) }
     | '[' ']'               { hoEmptyList }
     | '[' terms ']'         { homkList (reverse (hoEmptyList:$2)) }
     | '[' terms '|' term ']' { homkList (reverse ($4:$2)) }

terms  :: { [HoTerm] }
       : terms ',' term     { $3:$1 }
       | term               { [$1] }

goal :: { HoGoal }
     : atoms                { reverse $1 }

{


-- happyError :: Parser a
happyError = do
    loc <- getPSrcLoc
    buf <- getSrcBuf
    throwError $ strMsg ((show loc) ++ ": Parse error near " ++ (show (take 10 buf))) 

}
