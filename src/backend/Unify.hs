module Unify (unify) where

import Subst (combine, subst, bind, success)
import Core (CExpr(..), fv, Flex(..), isNamedCVar)
import Control.Monad (when, foldM)


-- unification

-- unify :: (Symbol a, Eq a, Monad m) => Expr a -> Expr a -> m (Subst a)

unify (CVar v1) e@(CVar v2)
    | v1 == v2  = return success
    | otherwise = case v1 of
                    Flex _ x   -> if isNamedCVar e
                                  then return (bind v1 e)
                                  else return success
                    AnonFlex _ -> return success      -- unifies but not produce binding

unify (CVar v) t =
   case v of
     Flex _ x ->  do
        occurCheck v t
        return (bind v t)
     AnonFlex _ -> return success

unify t1 t2@(CVar v) = unify t2 t1

unify (CApp _ e1 e2) (CApp _ e1' e2') = do
    s1 <- unify e1 e1'
    -- s2 <- unify (subst s1 e2) (subst s1 e2')
    s2 <- foldM (\s -> \(e,e') -> unify (subst s e) (subst s e')) s1 (zip e2 e2')
    return (s1 `combine` s2)

-- unify (Tup es) (Tup es') = listUnify es es'

unify (CPred _ p) (CPred _ q)
    | p == q    = return success
    | otherwise = fail "Unification fail"

unify (CCons e1 e2) (CCons e1' e2') = do
    s1 <- unify e1 e1'
    s2 <- unify (subst s1 e2) (subst s1 e2')
    return (s1 `combine` s2)

unify CNil CNil = return success

unify (CConst c1) (CConst c2)
    | c1 == c2 = return success
    | otherwise = fail "Unification fail"
unify (CNumber c1) (CNumber c2)
        | c1 == c2 = return success
        | otherwise = fail "Unification fail"

unify _ _ = fail "Should not happen"

-- occurCheck :: (Symbol a, Eq a, Monad m) => a -> Expr a -> m ()
occurCheck a e = when (a `occursIn` e) $ fail "Occur Check"
    where occursIn a e = a `elem` (fv e)
