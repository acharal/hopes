--  Copyright (C) 2013 Angelos Charalambidis  <a.charalambidis@di.uoa.gr>
--                     Emmanouil Koukoutos    <manoskouk@softlab.ntua.gr>
--                     Nikolaos S. Papaspyrou <nickie@softlab.ntua.gr>
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
 - The module implementing the core language polyH, as described in
 - (paper)
 - This language is used by the prover
 - Here, individuals/ functional applications have some extra subcases
 -     (lists, numbers) are are there to simplify pretty printing
 -     and some built-in predicates' implementation.
 -}

module Core where

import Pretty
import Basic
import Types

-- (unifiable) variables
data Flex a = Flex a Symbol
            | AnonFlex a
    deriving Functor

instance Pretty (Flex a) where
    ppr (Flex _ nm) = ppr nm
    ppr (AnonFlex _) = ppr "_"

-- AnonFlex are never equal
instance Eq (Flex a) where
    (Flex _ s1) == (Flex _ s2) = s1 == s2
    _ == _ = False

-- Expressions
data CExpr a = CTrue
             | CFail
             | CCut
             | CAnd    a (CExpr a) (CExpr a)
             | COr     a (CExpr a) (CExpr a)
             | CLambda a [Flex  a] (CExpr a)
             | CApp    a (CExpr a) [CExpr a]
             | CEq       (CExpr a) (CExpr a)
             | CExists a (Flex  a) (CExpr a)
             | CVar      (Flex  a)
             | CPred   a Symbol Int -- Predicate constant
             | CConst    Symbol
             | CNumber   (Either Integer Double)
             | CCons     (CExpr a) (CExpr a)
             | CNil
             -- lift :: forall t. o -> t
             -- For a predicate type t,
             --   lift(true) == T(t) and
             --   lift(fail) == _|_(t)
             | CLift   a (CExpr a)

    deriving Functor

-- predicate definitions
data CPredDef a = CPredDef { cPredName   :: Symbol    -- defined pred.
                           , cPredAr     :: Int       -- arity
                           , cPredTp     :: PolyType  -- type
                           , cPredIsMono :: Bool      -- mono- or polymorphic
                           , cPredCls    :: [CExpr a] -- defining clauses
                           }

-- Knowledge base is a list of predicate definitions
type KnowledgeBase a = [CPredDef a]

-- goals are simply expressions
type CGoal a = CExpr a

deriving instance Show a => Show (Flex a)
deriving instance Show a => Show (CExpr a)

-- Priorities:
--   0: λ,∃
--   1: ∨
--   2: ∧
--   3: =

instance Pretty (CExpr a) where
    ppr = pprCPrec 0

precQ = 0
precOr = 1
precAnd = 2
precEq = 3
precAll = 4

pprCPrec prec CTrue =
    text "true"
pprCPrec prec CFail =
    text "fail"
pprCPrec prec CCut  =
    char '!'
pprCPrec prec (CAnd _ ex1 ex2) =
    (if prec > precAnd then parens else id)
        ( pprCPrec precAnd ex1 <+>
          text "∧" <+>
          pprCPrec precAnd ex2
        )
pprCPrec prec (COr  _ ex1 ex2) =
    (if prec > precOr then parens else id)
        ( pprCPrec precOr ex1 <+>
          text "∨" <+>
          pprCPrec precOr ex2
        )
pprCPrec prec (CLambda _ vars bd) =
    char 'λ' <> parens ( hcat $ punctuate comma (map ppr vars) ) <>
    char '.' <+> pprCPrec precQ bd
pprCPrec prec (CApp _ func args) =
    pprCPrec precAll func <> parens ( hcat $ punctuate comma (map (pprCPrec 0) args) )
pprCPrec prec (CEq ex1 ex2) =
    (if prec > precEq then parens else id)
        ( pprCPrec precEq ex1 <>
          text "=" <>
          pprCPrec precEq ex2
        )
pprCPrec prec (CExists _ var expr) =
    char '∃' <> ppr var <> char '.' <+> pprCPrec precQ expr
pprCPrec prec (CVar flex) =
    ppr flex
pprCPrec prec (CPred _ nm ar) =
    text (nm ++ "/") <> int ar
pprCPrec prec (CConst nm) =
    text nm
pprCPrec prec (CNumber n) =
    ppr n
pprCPrec prec lst@(CCons hd tl) =
    brackets $ pprList lst
    where pprList (CCons hd CNil) = pprCPrec 0 hd
          pprList (CCons hd tl) =
              pprCPrec prec hd <> comma <> pprList tl
          pprList ex = pprCPrec 0 ex
pprCPrec prec CNil =
    text "[]"
pprCPrec prec (CLift _ expr) =
    char '↑' <> parens (pprCPrec 0 expr)

instance Pretty (CPredDef a) where
    ppr (CPredDef nm ar _ mono exps) =
        vcat $ map ppr' exps
        where ppr' expr = text (nm ++ "/") <> int ar <+>
                          (if mono then ppr ":-" else ppr "<-") <+>
                          ppr expr

instance Pretty [CPredDef a] where
  ppr lst = vcat $ map ppr lst

instance Pretty [CExpr a] where
  ppr lst = vcat $ map ppr lst

instance HasType a => HasType (Flex a) where
    typeOf (Flex a _  ) = typeOf a
    typeOf (AnonFlex a) = typeOf a
    hasType tp (Flex a nm) = Flex (hasType tp a) nm
    hasType tp (AnonFlex a) = AnonFlex $ hasType tp a


instance HasType a => HasType (CExpr a) where
    typeOf CTrue = Rho_pi Pi_o
    typeOf CFail = Rho_pi Pi_o
    typeOf CCut  = Rho_pi Pi_o
    typeOf (CAnd a _ _)    = typeOf a
    typeOf (COr  a _ _)    = typeOf a
    typeOf (CLambda a _ _) = typeOf a
    typeOf (CApp    a _ _) = typeOf a
    typeOf (CEq       _ _) = Rho_pi Pi_o
    typeOf (CExists a _ _) = typeOf a
    typeOf (CVar f)        = typeOf f
    typeOf (CPred   a _ _) = typeOf a
    typeOf (CLift a _ )    = typeOf a
    typeOf (CConst _ )     = Rho_i -- TODO: include func. type?
    typeOf _ = Rho_i

    hasType tp (CAnd a ex1 ex2)    = CAnd (hasType tp a) ex1 ex2
    hasType tp (COr  a ex1 ex2)    = COr  (hasType tp a) ex1 ex2
    hasType tp (CLambda a ex1 ex2) = CLambda (hasType tp a) ex1 ex2
    hasType tp (CApp    a ex1 ex2) = CApp    (hasType tp a) ex1 ex2
    hasType tp (CExists a ex1 ex2) = CExists (hasType tp a) ex1 ex2
    hasType tp (CVar f)            = CVar  (hasType tp f)
    hasType tp (CPred a ex1 ex2  ) = CPred (hasType tp a) ex1 ex2
    hasType _ ex = ex


instance Flatable (CExpr a) where
    flatten ex@(CAnd _ ex1 ex2) =
        ex : flatten ex1 ++ flatten ex2
    flatten ex@(COr _ ex1 ex2) =
        ex : flatten ex1 ++ flatten ex2
    flatten ex@(CEq ex1 ex2) =
        ex : flatten ex1 ++ flatten ex2
    flatten ex@(CCons ex1 ex2) =
        ex : flatten ex1 ++ flatten ex2
    flatten ex@(CLambda _ vars bd) =
        ex : map CVar vars ++ flatten bd
    flatten ex@(CApp _ func args) =
        ex : flatten func ++ concatMap flatten args
    flatten ex@(CExists _ var bd) =
        ex : CVar var : flatten bd
    flatten ex@(CLift _ ex') =
        ex : flatten ex'
    flatten ex =
        [ex]

allCVars ex = ex |> flatten
                 |> filter isNamedCVar
                 |> map (\(CVar f) -> f)


isNamedCVar (CVar (Flex _ _ ) ) = True
isNamedCVar _ = False
