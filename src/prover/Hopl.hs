module Hopl where

import HpSyn
import Pretty
import Loc

import List (nub)
import Control.Monad.State
import Control.Monad.Identity

type Var = String

data Term =
    Var Var             -- variable
  | Pre Var             -- predicate
  | Con String          -- constant of type i
  | Fun Var [Term]      -- function symbol
  | Uni Term Term       -- union of sets
  | Set [Term]          -- a set of terms
 deriving Eq

data Atom = Atom Term [Term]
 deriving Eq

type Clause = (Atom, [Atom])

type Prog = [Clause]

type Goal = [Atom]

emptyGoal :: Goal
emptyGoal = []

varsT :: Term -> [Var]
varsT (Var v)    = [v]
varsT (Fun _ tl) = nub (concatMap varsT tl)
varsT (Set tl)   = nub (concatMap varsT tl)
varsT (Uni t t') = (varsT t) ++ (varsT t')
varsT _          = []

varsA :: Atom -> [Var]
varsA (Atom (Var v) tl) = nub (v:(concatMap varsT tl))
varsA (Atom (Pre v) tl) = nub (concatMap varsT tl)
varsA _ = error "Not expected atom"

varsAs atoms = nub (concatMap varsA atoms)

varsC :: Clause -> [Var]
varsC (h,b) = varsAs (h:b)

instance Pretty Term where
    ppr (Var v) = text v
    ppr (Pre v) = text v
    ppr (Con c) = text c
    -- ppr (Fun "[]" tl) = bracket (sep [elems, tail])
    ppr (Fun v tl) = hcat [ text v, parens (sep (punctuate comma (map ppr tl))) ]
    ppr (Set tl)   = braces (sep (punctuate comma (map ppr tl)))

instance Pretty Atom where
    ppr (Atom t tl) = ppr t <> parens (sep $ punctuate comma (map ppr tl))

instance Show Term where
    showsPrec n a = showsPrec n (ppr a)

instance Show Atom where
    showsPrec n a = showsPrec n (ppr a)


-- simplifier :: HpSyn -> Hopl
type Simplifier = StateT Int Identity

runSimple m = runIdentity $ evalStateT m 0

simplifyProg :: HpSource -> Simplifier Prog
simplifyProg src = do
    clauses <- mapM simplifyClause (clauses src)
    return clauses

simplifyClause :: LHpClause -> Simplifier Clause
simplifyClause (L _ (HpClaus h b)) = do
    sh <- simplifyAtom h
    sb <- mapM simplifyAtom b
    return (sh, sb)

simplifyAtom (L _ (HpAtom e)) =
    let simpleExp2 (L _ (HpPar e)) = simpleExp2 e
        simpleExp2 (L _ (HpAnno e _)) = simpleExp2 e
        simpleExp2 (L _ (HpPred v)) = return (Pre v)
        simpleExp2 (L _ (HpTerm t)) = simplifyTerm t
        simpleExp2 (L _ (HpApp _ _)) = error ("unexpected application")
        simpleExp e@(L _ (HpApp _ _)) = 
            let (p, args) = splitExp e
            in do
            args' <- mapM simpleExp2 args
            p' <- simpleExp2 p
            return (Atom p' args')
        simpleExp e = do
            t' <- simpleExp2 e
            return (Atom t' [])
        splitExp (L _ (HpApp e el)) = (p, args ++ el)
            where (p, args) = splitExp e
        splitExp e@(L _ _) = (e, [])
    in simpleExp e

simplifyAtom (L _ HpCut) = error ("please do not use '!' for now")

simplifyTerm (L _ (HpVar v))
    | isBound v = return (Var v)
    | otherwise = return (Pre v)

simplifyTerm (L _ (HpCon c)) = return (Con c)
simplifyTerm (L _ (HpList [] Nothing)) = return (Con "[]")
simplifyTerm (L _ (HpList tl maytail)) = do
    ltai <- case maytail of
                Nothing -> return (Con "[]")
                Just t -> simplifyTerm t
    tl' <- mapM simplifyTerm tl
    return $ foldr (\x -> \y -> Fun "[]" [x,y]) ltai tl'

simplifyTerm (L _ (HpTup tl)) = do
    tl' <- mapM simplifyTerm tl
    return (Fun "" tl')

simplifyTerm (L _ (HpFun f tl)) = do
    tl' <- mapM simplifyTerm tl
    return (Fun f tl')

simplifyTerm (L _ HpWild) = freshVar

simplifyTerm (L _ (HpSet tl)) = do
    tl' <- mapM simplifyTerm tl
    return (Set tl')

simplifyTerm t = error (show t)

freshVar :: Simplifier Term
freshVar = do
    n <- get
    modify (+1)
    return (Var ("_"++show n))