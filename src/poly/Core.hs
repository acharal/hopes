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
import Loc (LocSpan)
import Data.List (union)
import Data.Maybe (catMaybes)

type VarSym   = Symbol
type ConstSym = Symbol
type TyVarSym = Symbol
type PredSym  = (Symbol, Int)

--type Info = Typed LocSpan -- Info { type :: Type , loc :: LocSpan }
type Info = RhoType

-- (unifiable) variables
data Flex = Flex Info VarSym
          | AnonFlex Info

instance Pretty Flex where
    ppr (Flex _ nm) = ppr nm
    ppr (AnonFlex _) = ppr "_"

-- AnonFlex are never equal
instance Eq Flex where
    (Flex _ s1) == (Flex _ s2) = s1 == s2
    _ == _ = False

-- Expressions
data CExpr   = CTrue
             | CFail
             | CCut
             | CAnd    Info CExpr CExpr
             | COr     Info CExpr CExpr
             | CLambda Info [Flex] CExpr
             | CApp    Info CExpr [CExpr]
             | CEq          CExpr CExpr
             | CExists Info Flex CExpr
             | CVar         Flex
             | CPred   Info Symbol Int -- Predicate constant
             | CConst       ConstSym
             | CNumber      (Either Integer Double)
             | CCons        CExpr CExpr
             | CNil
             | CNot         CExpr
             -- lift :: forall t. o -> t
             -- For a predicate type t,
             --   lift(true) == T(t) and
             --   lift(fail) == _|_(t)
             | CLift   Info CExpr
             deriving Eq

isVar :: CExpr -> Bool
isVar (CVar _) = True
isVar _ = False

elemVar :: VarSym -> [Flex] -> Bool
elemVar sym vs = sym `elem` (namedVars vs)

maybeNamedVar (Flex _ a) = Just a
maybeNamedVar (AnonFlex _) = Nothing
namedVars as = catMaybes $ map maybeNamedVar as

allCVars ex = ex |> flatten
                 |> filter isNamedCVar
                 |> map (\(CVar f) -> f)


isNamedCVar (CVar (Flex _ _ ) ) = True
isNamedCVar _ = False

fv :: CExpr -> [Flex]
fv (CVar a@(Flex _ _)) = [a]
fv (CVar _)     = []
fv (CPred _ _ _)= []
fv (CTrue)      = []
fv (CFail)      = []
fv (CCut)       = []
fv (CNumber _)  = []
fv (CConst _)   = []
fv (CNot e)      = fv e
fv (CApp _ e1 es)  = foldl union [] $ map fv (e1:es)
fv (CAnd _ e1 e2)  = fv e1 `union` fv e2
fv (COr  _ e1 e2)  = fv e1 `union` fv e2
fv (CEq  e1 e2)  = fv e1 `union` fv e2
fv (CLambda _ as e) = filter (`elem` as) $ fv e
fv (CExists _ a e) = filter (/= a) $ fv e
fv (CCons e1 e2) = fv e1 `union` fv e2
fv (CNil) = []

functor (CApp _ e es) = functor e
functor e = e

args (CApp _ e es) = args e ++ [es]
args _ = []

splitExist (CExists _ v e1) = ((v:vs), e')
    where (vs, e') = splitExist e1
splitExist e = ([], e)

splitLambda (CLambda _ v e1) = ((v:vs), e')
    where (vs, e') = splitLambda e1
splitLambda e = ([], e)

exists vs e = foldr (CExists (typeOf e)) e vs
-- lambda vs e = foldr CLambda e vs

-- predicate definitions
data CPredDef   = CPredDef { cPredName   :: Symbol    -- defined pred.
                           , cPredAr     :: Int       -- arity
                           , cPredTp     :: PolyType  -- type
                           , cPredIsMono :: Bool      -- mono- or polymorphic
                           , cPredCls    :: [CExpr]   -- defining clauses
                           }

-- Knowledge base is a list of predicate definitions
type KnowledgeBase = [CPredDef]

clausesOf :: KnowledgeBase -> PredSym -> [CExpr]
clausesOf base (p,n) =
  concatMap (\def -> cPredCls def) $ filter (\def -> cPredName def == p && cPredAr def == n) base

-- goals are simply expressions
type CGoal = CExpr

deriving instance Show Flex
deriving instance Show CExpr

-- Priorities:
--   0: λ,∃
--   1: ∨
--   2: ∧
--   3: =

instance Pretty CExpr where
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
  case n of
    Left n' -> ppr n'
    Right n' -> ppr n'
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

instance Pretty (CPredDef) where
    ppr (CPredDef nm ar _ mono exps) =
        vcat $ map ppr' exps
        where ppr' expr = text (nm ++ "/") <> int ar <+>
                          (if mono then ppr ":-" else ppr "<-") <+>
                          ppr expr

instance Pretty [CPredDef] where
  ppr lst = vcat $ map ppr lst

instance Pretty [CExpr] where
  ppr lst = vcat $ map ppr lst

instance HasType Flex where
    typeOf (Flex a _  ) = typeOf a
    typeOf (AnonFlex a) = typeOf a
    hasType tp (Flex a nm) = Flex (hasType tp a) nm
    hasType tp (AnonFlex a) = AnonFlex $ hasType tp a


instance HasType CExpr where
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


instance Flatable CExpr where
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
