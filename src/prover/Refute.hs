module Refute where

import Logic
import Hopl

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

import List

-- Substitutions 

type Subst = [(Var, Term)]  -- substitution list

epsilon :: Subst        -- identity substitution
epsilon = []


-- applying a substitution to various structures

appV :: Subst -> Var -> Term
appT :: Subst -> Term -> Term
appA :: Subst -> Atom -> Atom

appV [] v = Var v
appV ((v',t'):ss) v
    | v' == v   = t'
    | otherwise = appV ss v

appT s (Var v)    = appV s v

appT s (Fun f tl) = Fun f (map (appT s) tl)
appT s (Set tl)   = Set (map (appT s) tl)
appT s (Uni t t') = Uni (appT s t) (appT s t')
appT s t          = t

appA s (Atom t tl)= Atom (appT s t) (map (appT s) tl)

varS :: Subst -> [Var]
varS s = map fst s

-- composition of substitutions
comp :: Subst -> Subst -> Subst
comp s s' =
    let s1 = filter (\(a,b) -> b /= (Var a)) [ (v, appT s' t) | (v, t) <- s ]
        s2 = filter (\(a,b) -> a `notElem` (varS s)) s'
    in  s1 ++ s2


restrict :: [Var] -> Subst -> Subst
restrict vl sl = filter (\(v,_) -> v `elem` vl) sl

-- Unification 

--unify :: Term -> Term -> Maybe Subst
unify (Var "_") t = return epsilon
unify t (Var "_") = return epsilon

unify (Var v) (Var v')
    | v == v'   = return epsilon
    | otherwise = return [(v, Var v')]

unify (Var v) t = 
    if occurCheck v t then
        fail ("Variable "   ++ (show v) ++
              " occurs in " ++ (show t) )
    else
        return [(v,t)]

unify t (Var v) = unify (Var v) t

unify (Fun f tl) (Fun f' tl')
    | f == f'   = listUnify tl tl'
    | otherwise = fail ""

unify (Set tl) (Set tl') = listUnify tl tl'

unify t t' 
    | t == t'   = return epsilon
    | otherwise = fail ("Terms " ++ (show t) ++ 
                        " and "  ++ (show t') ++
                        " are not unifyable")

unifyAtoms a1@(Atom p pl) a2@(Atom q ql)
    | p == q    = listUnify pl ql
    | otherwise = fail ("Atoms " ++ (show a1) ++ 
                        " and "  ++ (show a2) ++
                        " are not unifyable")

-- listUnify :: [Term] -> [Term] -> Maybe Subst
listUnify [] [] = return epsilon
listUnify (x:xs) (y:ys) =  do
    s  <- unify x y
    s' <- listUnify (map (appT s) xs) (map (appT s) ys)
    return (s `comp` s')
listUnify lt lt' = fail ("Atoms " ++ (show lt) ++ 
                        " and "  ++ (show lt') ++
                        " are not unifyable")


occurCheck :: Var -> Term -> Bool
occurCheck v t = v `elem` (varsT t)


--type RefuteM a = LogicT (StateT Int Identity) a
type RefuteM a = StateT Int (LogicT Identity) a

prove :: Prog -> Goal -> RefuteM Subst
prove p g = do
    subst <- refute p g
    return (restrict (varsAs g) subst)

refute p [] = return epsilon
refute p g  =
    --trace ("Refute goal " ++ show g) $
    derive p g                  >>- \(g',s') ->
    refute p (map (normA.(appA s')) g') >>- \ans -> 
    return (s' `comp` ans)


clauses p (Atom v _) =
    let l = filter (\((Atom v' _), _) -> v' == v) p
    in  msum (map return l)

derive = resol

pickAtom :: Goal -> RefuteM (Atom, Goal)
pickAtom []     = fail "Empty goal, cannot select an atom"
pickAtom (a:as) = return (a,as)

resol p [] = return (emptyGoal,epsilon)
resol p g  =
    pickAtom g         >>- \(a,rest) ->
    clauses p a        >>- \c ->
    variant c          >>- \(h',b') ->
    unifyAtoms a h'    >>- \subst ->
    return (b'++rest, subst)

variant c@(h,b) =
    let subst = mapM (\v -> newVar >>= \n -> return (v, (Var n))) (varsC c)
    in do 
        s <- subst
        return (appA s h, map (normA.(appA s)) b)

-- hackia
normA (Atom (Con v) tl) = Atom (Pre v) tl
normA t = t

newVar :: RefuteM Var
newVar = do
    i <- get
    modify (+1)
    return ("_G"++(show i))
