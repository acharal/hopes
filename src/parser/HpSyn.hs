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

newtype HpSymbol = Sym String
    deriving Eq


{- -fallow-undecidable-instances
instance Symbol a => Pretty a where
    ppr a = ppr (text (symbolName a))
-}

data HpBuildin = 
      HpBList
    | HpBWild 
    | HpBNat 
    | HpBSet 
    | HpBCut
    deriving (Eq, Show)


instance Pretty HpSymbol where
    ppr (Sym s) = text s

instance Show HpSymbol where
    showsPrec p (Sym s) = showsPrec p s


data HpBinding  id = HpBind { symbolBind:: id,  postType::Type }
type HpBindings id = [HpBinding id]

data HpSource id =
    HpSrc { 
        clauses :: [LHpFormula id],
        tysigs  :: [LHpTySign id]
    }


-- normalized formula :  
-- forall x1 ... xk. (A1, ..., An <- B1, ..., Bm)
-- rule or clause has exactly one A an more than one B
-- fact has exactly one A and no B
-- goal has no A and no or more B
-- contradiction (False) has no A and no B

data HpFormula id = HpForm (HpBindings id) [LHpExpr id] [LHpExpr id]

data HpExpr id = 
      HpSym id                             -- symbol (constant, functional symbol, variable, predicate)
    | HpApp (LHpExpr id) [LHpExpr id]      -- general application (predicate or func sym)
    | HpPar (LHpExpr id)                   -- parenthesized expression
    | HpLam (HpBindings id) (LHpExpr id)   -- lambda abstraction
    | HpAnn (LHpExpr id) Type              -- type annotated expression
    | HpTup [LHpExpr id]                   -- tuple. can be defined as HpApp (HpSym "()") [LHpExpr]

type HpTySign id = (id,Type)

class Eq b => HasBindings a b where
    bindings :: a -> HpBindings b
    isBind :: a -> b -> Bool
    isBind a s = any (s==) $ map symbolBind (bindings a)

instance Eq a => HasBindings (HpFormula a) a where
    bindings (HpForm b _ _) = b

instance Eq a => HasBindings (HpExpr a) a where
    bindings (HpLam b _) = b
    bindings _ = []

instance (Eq b, HasBindings a b) => HasBindings (Located a) b where
    bindings = bindings . unLoc 

{-
hAtom :: LHpClause a -> LHpExpr a
hAtom le = let (HpC _ h _) = unLoc le in h

bAtoms :: LHpClause a -> [LHpExpr a]
bAtoms le = let (HpC _ _ b) = unLoc le in b

atomsOf :: LHpClause a -> [LHpExpr a]
atomsOf lc = (hAtom lc):(bAtoms lc)

fact :: LHpClause a -> Bool
fact = null.bAtoms
-}

isFact e = 
    case unLoc e of
        (HpForm _ [h] []) -> True
        _ -> False

lits :: LHpFormula a -> [LHpExpr a]
lits lf = 
    case unLoc lf of
        (HpForm _ hs bs) -> hs ++ bs


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

class HasSymbols a s where
    symbols :: a -> [s]

instance HasSymbols (HpExpr a) a where
    symbols (HpPar e)  = symbols e
    symbols (HpTup es) = concatMap symbols es
    symbols (HpSym s)  = [s]
    symbols (HpApp e1 e2) = symbols e1 ++ concatMap symbols e2
    symbols (HpAnn e _) = symbols e
    symbols (HpLam _ e) = symbols e

instance HasSymbols (HpFormula a) a where
    symbols (HpForm _ as bs) = concatMap symbols as ++ concatMap symbols bs

instance HasSymbols a s => HasSymbols (Located a) s where
    symbols = symbols . unLoc 

instance HasSymbols (HpSource a) a where
    symbols (HpSrc forms _) = concatMap symbols forms

{-- every symbol that is not bound by qualifiers (or lambda)
freeSym :: (Eq s, HasSymbols a s, HasBindings a s) => a -> [s]
freeSym e = filter (isBind e) (symbols e)
-}

class (Eq s, HasSymbols a s) => HasFreeSyms a s where
    freeSym :: a -> [s]

instance (Eq s) => HasFreeSyms (HpExpr s) s where
    freeSym e@(HpLam _ e') = filter (not.isBind e) (freeSym e)
    freeSym e = symbols e

instance (Eq s) => HasFreeSyms (HpFormula s) s where
    freeSym e@(HpForm _ hs bs) = nub $ filter (not.isBind e) (concatMap (freeSym.unLoc) (hs++bs))

instance (Eq s) => HasFreeSyms (HpSource s) s where
    freeSym e@(HpSrc forms _) = nub $ concatMap (freeSym.unLoc) forms

{-
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
-}


-- located syntax 
type LHpExpr a    = Located (HpExpr a)
type LHpFormula a = Located (HpFormula a)
type LHpTySign a  = Located (HpTySign a)

-- parsed located syntax 

type PLHpExpr    = LHpExpr    HpSymbol
type PLHpFormula = LHpFormula HpSymbol
type PLHpTySign  = LHpTySign  HpSymbol
type PHpSource   = HpSource   HpSymbol

type PLHpAtom   = PLHpExpr
type PLHpTerm   = PLHpExpr
type PLHpGoal   = PLHpFormula
type PLHpClause = PLHpFormula


 -- pretty printing 

instance Pretty a => Pretty (HpExpr a) where
    ppr (HpAnn e ty)  = hsep [ ppr (unLoc e), dcolon, ppr ty ]
    ppr (HpPar e)     = parens (ppr (unLoc e))
    ppr (HpSym s)     = ppr s
    ppr (HpApp e es)  = ppr (unLoc e) <> parens (sep (punctuate comma (map (ppr.unLoc) es)))
    ppr (HpTup es)    = parens (sep (punctuate comma (map (ppr.unLoc) es)))


instance Pretty a => Pretty (HpFormula a) where
    ppr (HpForm _ [h] []) = ppr (unLoc h) <> dot
    ppr (HpForm _ h b)  = 
        hang (  sep (punctuate comma (map (ppr.unLoc)  h)) <> entails) 4 $ 
                sep (punctuate comma (map (ppr.unLoc)  b)) <> dot


instance Pretty a => Pretty (HpSource a) where
    ppr src = vcat $ map (ppr.unLoc) (clauses src)

