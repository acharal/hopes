module HpSyn where

{-
    Higher order Prolog abstract syntax
-}

import Loc  (Located, unLoc)
import Char (isUpper)
import List (nub)
import Maybe(catMaybes)
import Pretty
import Types(Type)

type HpSymbol = String

data HpSource =
    HpSrc { 
        tysigs  :: [LHpTySign],
        clauses :: [LHpClause]
    }

{-
    Preliminaries
    1. Symbols
        1.1. Constants
        1.2. Function symbols
        1.3. Predicate symbols
        1.4  Variables
    2. Logical connectives (usually builtin)
        2.1 implication ":-"
        2.2 conjuction  ","
        2.3 disjuction  ";"
    3. Quantifiers (forall, exists)

    Basics

    1. a term is an expression of type i
    2. a literal is an expression of type o
    3. an atom is a positive literal
    4. a formula is literals connected by logical connectives. Is of type o.
        4.1 a formula is called closed if has no free variables, namely all
            variables occurs in the formula, are quantified (or bound by lambda abstraction).
    5. a clause is either a fact or a rule
    6. a rule is a *special* formula has only one positive literal, aka A <- B_1, ..., B_n.
       where A is called the head of the rule and [B_1, ..., B_n] (connected by conjuction)
       called the body of the rule.
    7. a fact is a bodyless rule.
    8. a goal is a *special* formula that has no positive literals, aka <- G_1, ..., G_n.
       an empty goal is known as *contradiction*.

    Convensions of Prolog

    1. variables are denoted by *symbols* where their first letter is capital.
    2. Every variables (as defined in 1.) that occurs in a clause is implied to be
       universally quantified.
-}

data HpClause = HpClaus [HpSymbol] LHpAtom [LHpAtom]

hAtom :: LHpClause -> LHpExpr
hAtom lc = 
    let (HpClaus _ h _) = unLoc lc
    in  h

bAtoms :: LHpClause -> [LHpExpr]
bAtoms lc = 
    let (HpClaus _ _ b) = unLoc lc
    in  b

atomsOf :: LHpClause -> [LHpExpr]
atomsOf lc = (hAtom lc):(bAtoms lc)

-- the set of the bounded/quantified variables of a clause

boundV :: LHpClause -> [HpSymbol]
boundV lc = 
    let (HpClaus vs _ _) = unLoc lc
    in  vs

fact :: LHpClause -> Bool
fact = null.bAtoms


data HpExpr  = 
      HpSym HpSymbol              -- symbol (constant, functional symbol, variable, predicate)
    | HpApp LHpExpr [LHpExpr]   -- general application (predicate or func sym)
    | HpPar LHpExpr             -- parenthesized expression
    | HpLam [HpSymbol] LHpExpr    -- lambda abstraction
    | HpAnn LHpExpr Type        -- type annotated expression
    | HpTup [LHpExpr]           -- tuple. can be defined as HpApp (HpSym "()") [LHpExpr]


type HpTySign  = (HpSymbol,Type)


type LHpAtom = LHpExpr
type LHpTerm = LHpExpr

data HpGoal = HpGoal [HpSymbol] [LHpAtom]
-- get the arguments of an application

argsOf :: LHpExpr -> [LHpExpr]
argsOf e = 
    case unLoc e of
        (HpApp e1 e2) -> argsOf e1 ++ e2
        _ -> []

-- get a head of an application

headOf :: LHpExpr -> LHpExpr
headOf e = 
    case unLoc e of
        (HpApp e1 _) -> headOf e1
        _ -> e


-- free variables ?? [HpSymbol]

symbolsE :: LHpExpr -> [HpSymbol]
symbolsE le = 
    case unLoc le of
        HpPar e -> symbolsE e
        HpTup es -> concatMap symbolsE es
        HpSym s -> [s]
        HpApp e1 e2 -> symbolsE e1 ++ concatMap symbolsE e2
        HpAnn e t -> symbolsE e
        _ -> []

symbolsC :: LHpClause -> [HpSymbol]
symbolsC c = concatMap symbolsE (atomsOf c)

symbols :: HpSource -> [HpSymbol]
symbols src = nub $ concatMap symbolsC $ clauses src

isSymbol :: LHpExpr -> Bool
isSymbol e = 
    case unLoc e of 
        (HpSym _) -> True
        _ -> False

freeSymC :: LHpClause -> [HpSymbol]
freeSymC cl = filter (\x-> x `notElem` binds) (symbolsC cl)
    where binds = boundV cl

freeSymSrc :: HpSource -> [HpSymbol]
freeSymSrc src = concatMap freeSymC (clauses src)


-- located syntax 

type LHpSource = Located HpSource
type LHpClause = Located HpClause
type LHpTySign = Located HpTySign
type LHpExpr   = Located HpExpr
type LHpGoal   = Located HpGoal


 -- pretty printing 

instance Pretty HpExpr where
    ppr (HpAnn e ty)  = hsep [ ppr (unLoc e), dcolon, ppr ty ]
    ppr (HpPar e)     = parens (ppr (unLoc e))
    ppr (HpSym s)     = text s
    ppr (HpApp e es)  = ppr (unLoc e) <> parens (sep (punctuate comma (map (ppr.unLoc) es)))
    ppr (HpTup es)    = parens (sep (punctuate comma (map (ppr.unLoc) es)))
{-
instance Pretty HpTerm where
    ppr (HpId name)  = text name
    ppr (HpVar v)    = text v
    ppr (HpCon v)    = text v
    ppr (HpFun f tl) = text f <> parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr  HpWild      = char '_'
    ppr (HpTup tl)   = parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpSet tl)   = braces (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpList tl tail) = brackets $ sep $ (punctuate comma (map (ppr.unLoc) tl)) ++ ppr_tail
        where ppr_tail =
                case tail of
                    Nothing -> []
                    Just t  -> [text "|" <+> ppr (unLoc t)]
-}

instance Pretty HpClause where
    ppr (HpClaus _ h []) = ppr (unLoc h) <> dot
    ppr (HpClaus _ h b)  = 
        hang (ppr (unLoc h) <> entails) 4 $ 
                sep (punctuate comma (map (ppr.unLoc) b)) <> dot

instance Pretty HpSource where
    ppr src = vcat $ map (ppr.unLoc) (clauses src)

