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

{-
    Missing Features.
    - Special directives op/3, dynamic/2, ...
    - Operators
    - Commands (e.g. :- command)
    - Semicolon as 'or' operator
    - Strings ? "asdad"
-}

{
module Parser (
        module Parser,
        module ParseUtils
) where

import Lexer
import Syntax
-- import Symbol
import Buildins
import Types
import Loc
import ParseUtils
import Control.Monad.State
}

%tokentype { (Located Token) }
%token 
      ':-'      { (L _ TKgets)    }
      '.'       { (L _ TKdot)     }
      ','       { (L _ TKcomma)   }
      '('       { (L _ TKoparen)  }
      ')'       { (L _ TKcparen)  }
      '['       { (L _ TKobrak)   }
      '{'       { (L _ TKocurly)  }
      '}'       { (L _ TKccurly)  }
      ']'       { (L _ TKcbrak)   }
      '|'       { (L _ TKvert)    }
      '_'       { (L _ TKwild)    }
      '::'      { (L _ TKcolcol)  }
      '->'      { (L _ TKarrow)   }
      '\\'      { (L _ TKbslash)  }
      '!'       { (L _ TKcut)     }
      ';'       { (L _ TKsemi)    }
      ID        { (L _ (TKid _))  }
      '\''      { (L _ (TKsq))    }
      '='       { (L _ (TKeq))    }

%name parseSrc  src
%name parseGoal goal
%lexer { lexer  } { (L _ TKEOF) }
%monad { ParserT IO } { (>>=) } { return }

%right '->'
%left '::'
%left ID

%% 

src :: { PHpSrc }
    : stmts                     {% mkSrc (reverse $1) }

stmt :: { HpStmt HpSymbol }
     : clause                   { Left  $1 }
     | tysig                    { Right $1 }

stmts :: { [HpStmt HpSymbol] }
      : stmts stmt              { $2:$1 }
      |                         { [] }

clause :: { PLHpClause }
       : rule                   { $1 }
       | fact                   { $1 }

clauses :: { [PLHpClause] }
        : clauses clause        { $2:$1 }
        |                       { [] }

fact :: { PLHpClause }
     : atom '.'                 { located ($1,$>) $ mkQuantForm [$1] [] }

rule :: { PLHpClause }
     : atom ':-' body '.'       { located ($1,$>) $ mkQuantForm [$1] $3 }

body :: { [PLHpAtom] }
     : conj                     { reverse $1 }

conj :: { [PLHpAtom] }
     : conj ',' atom            { $3:$1 }
     | atom                     { [$1] }

atom :: { PLHpAtom }
     : exp                      { $1 }
     | '!'                      { located $1      $ mkBuildin "!" }

exp :: { PLHpExpr }
    : '(' exp ')'               { located ($1,$>) $ HpPar $2 }
    | exp tyann                 { located ($1,$>) $ HpAnn $1 (unLoc $2) }
    | ID                        { located  $1     $ (mkSym $1) }
    | exp '(' exps2 ')'         { located ($1,$>) $ HpApp $1 (reverse $3) }
    {-- | exp '=' exp               { located ($1,$>) $ HpApp univ [$1, $3]   } --}

exp2 :: { PLHpExpr }
    : term                      { $1 }
    |'(' exp2 ')'               { located ($1,$>) $ HpPar $2 }
    | exp2 tyann                { located ($1,$>) $ HpAnn $1 (unLoc $2) }

exps2 :: { [PLHpExpr] }
     : exps2 ',' exp2           { $3:$1 }
     | exp2                     { [$1] }

term :: { PLHpTerm }
    : name                      { $1 }
    | '_'                       { located $1      $ wildcat }
    | name '(' terms ')'        { located ($1,$>) $
                                    HpApp $1 (reverse $3) }
    | '(' terms2 ')'            { located ($1,$>) $ HpTup (reverse $2) }
    | '[' ']'                   { located ($1,$>) $ mkList []           Nothing   }
    | '[' terms ']'             { located ($1,$>) $ mkList (reverse $2) Nothing   }
    | '[' terms '|' term ']'    { located ($1,$>) $ mkList (reverse $2) (Just $4) }
{-
    | '{' '}'                   { located ($1,$>) $ HpSet [] }
    | '{' terms '}'             { located ($1,$>) $ HpSet $2 }
-}

terms :: { [PLHpTerm] }
      : terms ',' term          { $3:$1 }
      | term                    { [$1] }

terms2 :: { [PLHpTerm] }
       : terms2 ',' term        { $3:$1 }
       | term ',' term          { $3:$1:[] }

name :: { PLHpExpr }
     : ID                       { located $1      $ (mkSym $1) }
     | '\'' ID '\''             { located ($1,$>) $ (mkSym $2) }

goal :: { PLHpClause }
     : conj '.'                 { located bogusLoc $ mkQuantForm [] (reverse $1) }

type :: { LHpType }
     : ID                       { located $1      $ HpTyGrd (tokId $1) }
     | type '->' type           { located ($1,$>) $ HpTyFun $1 $3 }
     | '(' types ')'            { located ($1,$>) $ HpTyTup (reverse $2) }
     | '{' type '}'             { located ($1,$>) $ HpTyRel $2 }

types :: {  [LHpType]  }
      : types ',' type          { $3:$1 }
      | type                    { [$1]  }

tysig :: { TySig a }
      : ID tyann                { ((tokSym $1),(unLoc $2)) }

tyann :: { Located Type }
      : '::' type               {% mkTyp $2 >>= \t -> return $ located ($1,$>) t }

{
happyError = parseError'
}
