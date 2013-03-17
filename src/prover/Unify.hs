module Unify (unify) where

import Subst (combine, subst, bind, success)
import CoreLang (Expr(..), fv)
import Control.Monad (when)


-- unification

-- unify :: (Symbol a, Eq a, Monad m) => Expr a -> Expr a -> m (Subst a)

unify (Var v1) e@(Var v2)
    | v1 == v2  = return success
    | otherwise = return (bind v1 e)

unify (Var v) t = do
    occurCheck v t
    return (bind v t)

unify t1 t2@(Var v) = unify t2 t1

unify (App e1 e2) (App e1' e2') = do
    s1 <- unify e1 e1'
    s2 <- unify (subst s1 e2) (subst s1 e2')
    return (s1 `combine` s2)

-- unify (Tup es) (Tup es') = listUnify es es'

unify (Rigid p) (Rigid q)
    | p == q    = return success
    | otherwise = fail "Unification fail"

unify (ListCons e1 e2) (ListCons e1' e2') = do
    s1 <- unify e1 e1'
    s2 <- unify (subst s1 e2) (subst s1 e2')
    return (s1 `combine` s2)
    
unify ListNil ListNil = return success

unify _ _ = fail "Should not happen"

-- occurCheck :: (Symbol a, Eq a, Monad m) => a -> Expr a -> m ()
occurCheck a e = when (a `occursIn` e) $ fail "Occur Check"
    where occursIn a e = a `elem` (fv e)

