--  Copyright (C) 2007 2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Syntax where

{-
    Higher order Prolog abstract syntax
-}
import Symbol
import Loc
import Pretty
import Types
import Data.Monoid
import List (find)
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

type HpSymbol = Symbol String

consSym  = HpSym $ Sym ":"
nilSym   = HpSym $ Sym "[]"
cutSym   = HpSym $ Sym "!"
succSym  = HpSym $ Sym "s"
zeroSym  = HpSym $ Sym "0"
wildcat  = HpSym $ AnonSym



data HpBinding  a = HpBind { symbolBind :: !a,  postType :: Type }  deriving Eq
type HpBindings a = [HpBinding a]
-- type HpBindings a = Set.Set (HpBinding a)

lookupBind :: Eq a => a -> HpBindings a -> Maybe (HpBinding a)
lookupBind x = find ((x==).symbolBind)

type HpTySign = TySig HpSymbol

data HpProg a =
    HpProg { 
        clauses :: [LHpFormula a],
        ptysigs :: [HpTySign]
    }

tysigs p = ptysigs p `mappend` buildinsigs
    where buildinsigs = zip (buildinSym) (map buildinTyp buildinSym)


instance (Eq a, HasSignature a a) => HasSignature (HpProg a) a where
    sig p = (a, mempty)
        where (a, b) = mconcat (map sig (clauses p))

instance (Eq a, HasSignature a a) => HasSignature (HpFormula a) a where
    sig f@(HpForm b xs ys) = (filter (not.isBind f) as, bs `mappend` (map symbolBind b))
        where (as, bs) = mconcat $ map sig (xs ++ ys)

instance (Eq a, HasSignature a a) => HasSignature (HpExpr a) a where
    sig (HpSym s)    = sig s
    sig (HpApp e es) = mconcat (map sig (e:es))
    sig (HpPar e)    = sig e
    sig (HpAnn e _)  = sig e
    sig (HpTup es)   = mconcat (map sig es)
    sig a@(HpLam b e)= (filter (not.isBind a) as, bs `mappend` (map symbolBind b))
        where (as, bs) = sig e



instance HasSignature a s => HasSignature (Located a) s where
    sig = sig . unLoc

-- returns the signature of a program

-- normalized formula :  
-- forall x1 ... xk. (A1, ..., An <- B1, ..., Bm)
-- rule or clause has exactly one A an more than one B
-- fact has exactly one A and no B
-- goal has no A and no or more B
-- contradiction (False) has no A and no B

data HpFormula a = HpForm (HpBindings a) [LHpExpr a] [LHpExpr a]

data HpExpr a = 
      HpSym a                              -- symbol (constant, functional symbol, variable, predicate)
    | HpApp (LHpExpr a) [LHpExpr a]        -- general application (predicate or func sym)
    | HpPar (LHpExpr a)                    -- parenthesized expression
    | HpLam (HpBindings a) (LHpExpr a)     -- lambda abstraction
    | HpAnn (LHpExpr a) Type               -- type annotated expression
    | HpTup [LHpExpr a]                    -- tuple. can be defined as HpApp (HpSym "()") [LHpExpr]
    deriving Eq


class Eq b => HasBindings a b where
    binds :: a -> HpBindings b
    isBind :: a -> b -> Bool
    isBind a s = any (s==) $ map symbolBind (binds a)

instance Eq a => HasBindings (HpFormula a) a where
    binds (HpForm b _ _) = b

instance Eq a => HasBindings (HpExpr a) a where
    binds (HpLam b _) = b
    binds _ = []

instance (Eq b, HasBindings a b) => HasBindings (Located a) b where
    binds = binds . unLoc 


isFact e = 
    case unLoc e of
        (HpForm _ [h] []) -> True
        _ -> False

isApp e =
    case unLoc e of
        (HpApp _ _) -> True
        _ -> False

argsOf :: LHpExpr a -> [LHpExpr a]
argsOf e = 
    case unLoc e of
        (HpApp e1 e2) -> argsOf e1 ++ e2
        _ -> []

-- get a head of an application

funcOf :: LHpExpr a -> LHpExpr a
funcOf e = 
    case unLoc e of
        (HpApp e1 _) -> funcOf e1
        _ -> e


isSymbol :: LHpExpr a -> Bool
isSymbol e = 
    case unLoc e of 
        (HpSym _) -> True
        _ -> False


-- located syntax 
type LHpExpr a    = Located (HpExpr a)
type LHpFormula a = Located (HpFormula a)

-- parsed located syntax 

type PLHpExpr    = LHpExpr    HpSymbol
type PLHpFormula = LHpFormula HpSymbol
type PHpProg     = HpProg     HpSymbol

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


instance Pretty a => Pretty (HpProg a) where
    ppr p = vcat $ map (ppr.unLoc) (clauses p)

