{
module Parser where

import Lexer
import HpSyntax
import Utils
import ParseUtils

}

%tokentype { (Located Token) }
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
      '|'       { (L _ TKvert )  }
      '_'       { (L _ TKwild )  }
      ID        { (L _ (TKid   $$)) }

%monad { Parser } { >>= } { return }
%lexer { lexer } { (L _ TKEOF) }
%name parseProg  prog
%name parseGoal  goal
%lexer { lexer  } { (L _ TKEOF) }
%monad { Parser } { >>= } { return }

%right '->'


%% 

src :: { HpSrc }
    : stmts                     { HpSrc (reverse $1) }

stmt :: { LHpStmt }
     : clause                   { L (getLoc $1)     $ HpCl $1 }
     | tysig                    { L (getLoc $1)     $ HpTS $1 }

stmts :: { [LHpStmt] }
      : stmts stmt              { $2:$1 }
      |                         { [] }

clause :: { LHpClaus }
       : rule                   { $1 }
       | fact                   { $1 }

clauses :: { [LHpClaus] }
        : clauses clause        { $2:$1 }
        |                       { [] }

fact :: { LHpClaus }
     : atom '.'                 { L (combLoc $1 $>) $ HpClaus $1 [] }

rule :: { LHpClaus }
     : atom ':-' body '.'       { L (combLoc $1 $>) $ HpClaus $1 $3 }

term :: { HoTerm }
     : ID                    { if isVar $1 then HoVar $1 else if (all isDigit $1) then homkNum ((read $1)::Int) else HoConst $1 }
     | '_'                   { HoVar "_" }
     | ID '(' terms ')'      { HoFun $1 (reverse $3) }
     | '[' ']'               { hoEmptyList }
     | '[' terms ']'         { homkList (reverse (hoEmptyList:$2)) }
     | '[' terms '|' term ']' { homkList (reverse ($4:$2)) }

conj :: { [LHpAtom] }
     : conj ',' atom            { $3:$1 }
     | atom                     { [$1] }

atom :: { LHpAtom }
     : ID                       { L (getLoc $1)     $ HpAtom (getName $1) [] }
     | ID '(' exps ')'          { L (combLoc $1 $>) $ HpAtom (getName $1) (reverse $3) }
     | '!'                      { L (getLoc $1)     $ HpCut }

exp  :: { LHpExpr }
     : term                     { L (getLoc $1)     $ HpTerm $1 }
     | '(' exp ')'              { L (combLoc $1 $>) $ HpPar  $2 }
     | exp tyann                { L (combLoc $1 $2) $ HpTyExp $1 $2 }
--     | exp term               { undefined } --shift/reduce with `ID ( terms )` rule

exps :: { [LHpExpr] }
     : exps ',' exp             { $3:$1 }
     | exp                      { [$1] }

term :: { LHpTerm }
     : ID                       { L (getLoc $1)     $ HpId (getName $1) }
     | '_'                      { L (getLoc $1)     $ HpWild }
     | ID '(' terms ')'         { L (combLoc $1 $>) $ HpFun (getName $1) (reverse $3) }
     | '(' terms2 ')'           { L (combLoc $1 $>) $ HpTup (reverse $2) }
     | '[' ']'                  { L (combLoc $1 $>) $ HpList []           Nothing   }
     | '[' terms ']'            { L (combLoc $1 $>) $ HpList (reverse $2) Nothing   }
     | '[' terms '|' term ']'   { L (combLoc $1 $>) $ HpList (reverse $2) (Just $4) }
     | '{' '}'                  { L (combLoc $1 $>) $ HpSet [] }
     | '{' terms '}'            { L (combLoc $1 $>) $ HpSet $2 }

terms :: { [LHpTerm] }
      : terms ',' term          { $3:$1 }
      | term                    { [$1] }

terms2 :: { [LHpTerm] }
       : terms2 ',' term        { $3:$1 }
       | term ',' term          { $3:$1:[] }

goal :: { HpGoal }
     : conj                     { reverse $1 }

type :: { LHpType }
     : ID                       { L (getLoc $1)     $ HpTyGrd (getName $1) }
     | type '->' type           { L (combLoc $1 $>) $ HpTyFun $1 $3 }
     | '(' types ')'            { L (combLoc $1 $>) $ HpTyTup (reverse $2) }
     | '{' type '}'             { L (combLoc $1 $>) $ HpTyRel $2 }

types :: {  [LHpType]  }
      : types ',' type          { $3:$1 }
      | type                    { [$1]  }

tysig :: { LHpTySig }
      : ID tyann                { L (combLoc $1 $>) $ HpTySig (getName $1) $2 }

tyann :: { LHpType }
      : '::' type               { $2 }

{
happyError = do
    ltok <- getLastTok
    tok <- getTok
    buf <- getSrcBuf
    case unLoc tok of
        TKEOF -> throwError $ ParseError (spanBegin (getLoc tok)) (text "Unexpected end of input")
        _     -> throwError $ ParseError (spanBegin (getLoc tok)) (text ("Parse error in input " ++ show (unLoc tok)))
}
