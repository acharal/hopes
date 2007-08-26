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

newtype HpSymbol = Sym String deriving Eq

class Symbol a where
    symbolName :: a -> String

instance Symbol HpSymbol where
    --symbolName :: HpSymbol -> String
    symbolName (Sym s) = s



{- -fallow-undecidable-instances
instance Symbol a => Pretty a where
    ppr a = ppr (text (symbolName a))
-}

data BuildIn = HpList | HpWild | HpNat | HpSet | HpCut

buildinSymbol :: BuildIn -> HpSymbol
buildinSymbol HpList = Sym "[]"
buildinSymbol HpWild = Sym "_"
buildinSymbol HpNat  = Sym "_s"
buildinSymbol HpSet  = Sym "{}"
buildinSymbol HpCut  = Sym "!"

instance Pretty HpSymbol where
    ppr (Sym s) = text s

instance Show HpSymbol where
    showsPrec p (Sym s) = showsPrec p s


data HpBinding id = HpBind id
type HpBindings id = [HpBinding id]

binds (HpBind x) = x

data HpSource id =
    HpSrc { 
        clauses :: [LHpClause id],
        tysigs  :: [LHpTySign id]
    }


data HpClause a = HpC (HpBindings a) (LHpAtom a) [LHpAtom a]
data HpGoal a   = HpG (HpBindings a) [LHpAtom a]

class HasBindings a b where
    bindings :: a -> HpBindings b

instance HasBindings (HpClause a) a where
    bindings (HpC b _ _) = b

instance HasBindings (HpGoal a) a where
    bindings (HpG b _) = b

hAtom :: LHpClause a -> LHpExpr a
hAtom le = let (HpC _ h _) = unLoc le in h

bAtoms :: LHpClause a -> [LHpExpr a]
bAtoms le = let (HpC _ _ b) = unLoc le in b

atomsOf :: LHpClause a -> [LHpExpr a]
atomsOf lc = (hAtom lc):(bAtoms lc)

fact :: LHpClause a -> Bool
fact = null.bAtoms


data HpExpr id = 
      HpSym id                          -- symbol (constant, functional symbol, variable, predicate)
    | HpApp (LHpExpr id) [LHpExpr id]   -- general application (predicate or func sym)
    | HpPar (LHpExpr id)                -- parenthesized expression
    | HpLam (HpBindings id) (LHpExpr id)   -- lambda abstraction
    | HpAnn (LHpExpr id) Type           -- type annotated expression
    | HpTup [LHpExpr id]                -- tuple. can be defined as HpApp (HpSym "()") [LHpExpr]


type HpTySign id = (id,Type)


type LHpAtom a = LHpExpr a
type LHpTerm a = LHpExpr a


argsOf :: LHpExpr a -> [LHpExpr a]
argsOf e = 
    case unLoc e of
        (HpApp e1 e2) -> argsOf e1 ++ e2
        _ -> []

-- get a head of an application

headOf :: LHpExpr a -> LHpExpr a
headOf e = 
    case unLoc e of
        (HpApp e1 _) -> headOf e1
        _ -> e


-- free variables ?? [HpSymbol]

symbolsE :: LHpExpr a -> [a]
symbolsE le = 
    case unLoc le of
        HpPar e -> symbolsE e
        HpTup es -> concatMap symbolsE es
        HpSym s -> [s]
        HpApp e1 e2 -> symbolsE e1 ++ concatMap symbolsE e2
        HpAnn e t -> symbolsE e
        _ -> []

symbolsC :: LHpClause a -> [a]
symbolsC c = concatMap symbolsE (atomsOf c)

symbols :: Eq a =>  HpSource a -> [a]
symbols src = nub $ concatMap symbolsC $ clauses src

isSymbol :: LHpExpr a -> Bool
isSymbol e = 
    case unLoc e of 
        (HpSym _) -> True
        _ -> False

freeSymC :: Eq a => LHpClause a -> [a]
freeSymC cl = filter (\x-> x `notElem` b) (symbolsC cl)
    where b = map binds (bindings (unLoc cl))

freeSymSrc :: Eq a => HpSource a -> [a]
freeSymSrc src = concatMap freeSymC (clauses src)


-- located syntax 

type LHpClause a = Located (HpClause a)
type LHpGoal a   = Located (HpGoal a)
type LHpTySign a = Located (HpTySign a)
type LHpExpr a   = Located (HpExpr a)



 -- pretty printing 

instance Pretty a => Pretty (HpExpr a) where
    ppr (HpAnn e ty)  = hsep [ ppr (unLoc e), dcolon, ppr ty ]
    ppr (HpPar e)     = parens (ppr (unLoc e))
    ppr (HpSym s)     = ppr s
    ppr (HpApp e es)  = ppr (unLoc e) <> parens (sep (punctuate comma (map (ppr.unLoc) es)))
    ppr (HpTup es)    = parens (sep (punctuate comma (map (ppr.unLoc) es)))


instance Pretty a => Pretty (HpClause a) where
    ppr (HpC _ h []) = ppr (unLoc h) <> dot
    ppr (HpC _ h b)  = 
        hang (ppr (unLoc h) <> entails) 4 $ 
                sep (punctuate comma (map (ppr.unLoc) b)) <> dot

instance Pretty a => Pretty (HpSource a) where
    ppr src = vcat $ map (ppr.unLoc) (clauses src)

