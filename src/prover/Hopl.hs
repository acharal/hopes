module Hopl where

import Syntax
import Pretty
import Loc

import List (nub)
import Char (isDigit)
import Control.Monad.State
import Control.Monad.Identity

type Var = String

data Term =
    Var Var             -- variable
  | Pre Var             -- predicate
  | Con String          -- constant of type i
  | Fun Var [Term]      -- function symbol
  | Set [[Term]] [Var]  -- a set of terms
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
varsT (Set tl vl) = nub $ (concatMap varsT (concat tl)) ++ vl
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
    ppr t@(Fun "[]" tl) = brackets (hsep [ppr_elems, ppr_tail])
	where elems (Fun "[]" [x,y]) = x:(elems y)
              elems t = []
              tails (Fun "[]" [x,y]) = tails y
              tails (Con "[]") = Nothing
              tails t = Just t 
              ppr_elems = sep (punctuate comma (map ppr (elems t)))
              ppr_tail = case tails t of 
                              Nothing -> empty
                              Just t -> text "|" <+> ppr t
    ppr t@(Fun "s" [x]) = 
        let countS (Fun "s" [x]) = (1 + a, y)
                where (a, y) = countS x
            countS t = (0, t)
        in case countS t of
            (s, Con "0") -> int s
            (s, t) -> hsep [int s, text "+", ppr t]
    ppr (Fun v tl) = hcat [ text v, parens (sep (punctuate comma (map ppr tl))) ]
    ppr (Set tl vl)   = braces (sep (punctuate (text "|") (sep (punctuate comma (map ppr_tup tl)):(map text vl))))
        where ppr_tup [t] = ppr t
              ppr_tup [] = empty
              ppr_tup l = parens $ sep $ punctuate comma (map ppr l)

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

simplifyGoal :: LHpGoal -> Simplifier Goal
simplifyGoal (L _ goal) = mapM simplifyAtom goal

simplifyAtom (L _ (HpAtom e)) =
    let simpleExp2 (L _ (HpPar e)) = simpleExp2 e
        simpleExp2 (L _ (HpAnno e _)) = simpleExp2 e
        simpleExp2 (L _ (HpPred v)) =
            if bound v then return (Var v) else return (Pre v)
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
    | bound v = return (Var v)
    | otherwise = return (Pre v)

simplifyTerm (L _ (HpCon c))
    | all isDigit c = let n = read c ::Int in return $ foldr (\x -> \y -> Fun "s" [y]) (Con "0") (replicate n 0)
    | otherwise = return (Con c)
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

{-
simplifyTerm (L _ (HpSet tl)) = do
    tl' <- mapM simplifyTerm tl
    return (Set tl' [])
-}

simplifyTerm t = error (show t)

freshVar :: Simplifier Term
freshVar = do
    n <- get
    modify (+1)
    return (Var ("_"++show n))
