--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Infer where

import Hopl
import KnowledgeBase
import Subst
import Logic

import Types
import Symbol

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Monoid
import List (last)


type Infer a = ReaderT (KnowledgeBase a) (StateT Int (LogicT Identity))

runInfer p m = runIdentity $ runLogic Nothing $ evalStateT (runReaderT m p) 0

infer :: KnowledgeBase a -> Infer a b -> Maybe (b, Infer a b)
infer p m =  runIdentity $ observe $ evalStateT (runReaderT (msplit m) p) 0

-- try prove a formula by refutation
-- prove  :: Goal a -> Infer a (Subst a)
prove g =  do
    ans <- refute g
    return (restrict (vars g) ans)

-- do a refutation
-- refute :: Goal a -> Infer a (Subst a)
refute g 
    | isContra g = return success
    | otherwise  = derive g >>- \(g',  s)  ->
                   refute (subst s g') >>- \ans ->
                   return (s `combine` ans)

-- a derivation
-- derive :: Goal a -> Infer a (Goal a, Subst a)
derive [] = return (contradiction, success)
derive g = 
    let f g a = case functor a of
                 Rigid _ -> resolve g a
                 Flex _  -> resolvF g a
                 Set _ _ -> resolvS g a
                 _ -> fail "Cannot derive anything from that atom"
    in split g  >>- \(a, g') -> f g' a



-- derive by resolution (the common rigid case)
-- FIXME: clause assumed to be a tuple.
--        goal assumed to be equivalent to the body of a clause
-- resolve :: Goal a -> Expr a -> Infer a (Goal a, Subst a)
resolve g e = 
    clausesOf e >>- \c     ->
    variant c   >>- \(h,b) ->
    unify e h   >>- \s     ->
    return (b `mappend` g, s)


resolvF g (App fv@(Flex v) es) = resolvS g (App (liftSet fv) es)


resolvS g (App (Set ss vs) e) = do
    let v = last vs             -- SEARCH ME: any solutions lost? discard all variables except the last one (which is continuous?)
    let TyFun a r = typeOf v
    x  <- freshIt v
    let x' = typed a (unTyp x)
    (Flex x') `waybelow` e >>- \s -> do
        v' <- freshIt v
        return (g, (bind v (Set [(Flex x')] [v'])) `combine` s)

-- unification

unify :: (Eq a, Monad m) => Expr a -> Expr a -> m (Subst a)

unify (Flex v1) e@(Flex v2)
    | v1 == v2  = return success
    | otherwise = return (bind v1 e)

unify (Flex v) t = do
    occurCheck v t
    return (bind v t)

unify t1 t2@(Flex v) = unify t2 t1

unify (App e1 e2) (App e1' e2') = do
    s1 <- unify e1 e1'
    s2 <- unify (subst s1 e2) (subst s1 e2')
    return (s1 `combine` s2)

unify (Tup es) (Tup es') = listUnify es es'

unify (Rigid p) (Rigid q)
    | p == q    = return success
    | otherwise = fail "Unification fail"

unify _ _ = fail "Should not happen"


listUnify  :: (Eq a, Monad m) => [Expr a] -> [Expr a] -> m (Subst a)
listUnify [] [] = return success
listUnify (e1:es1) (e2:es2) = do
    s <- unify e1 e2
    s' <- listUnify (map (subst s) es1) (map (subst s) es2)
    return (s `combine` s')

listUnify _ _   = fail "lists of different length"

occurCheck :: (Eq a, Monad m) => a -> Expr a -> m ()
occurCheck a e = when (a `occursIn` e) $ fail "Occur Check"

occursIn a e = a `elem` (vars e)


{-
    (waybelow x y) successed with a substitution if x is waybelow of y
    waybelow can successed more than once e.g. for all x that are waybelow y
    waybelow fails if no substitution exists to make x waybelow y
-}
-- waybelow :: MonadLogic m => Expr a -> Expr a -> m Subst
-- if p is higher order we want a finitary subset S
-- if p is zero order just unify x with p


waybelow (Flex x) (Rigid p)
    | order p == 0 = unify (Flex x) (Rigid p)
    | otherwise    = error "last to implement"
        -- prove (p(X1, ..., XN)) = [s1, s2, ...]

-- possibly a function symbol application (remember no partial applications allowed, so that can't be higher order)
waybelow e1@(Flex _) e2@(App _ _) = unify e1 e2

waybelow (Flex x) (Set sl vs@(v:_)) = do
    v' <- freshIt v
    return $ bind v (Set [] [x, v'])

waybelow e1@(Flex _) e2@(Flex v)
    | order v == 0 = unify e1 e2
    | otherwise    = waybelow e1 (liftSet e2)

waybelow (Flex x) e@(Tup es) = do
    xs <- mapM (\e -> freshIt x) es
    let xs' = Tup (map Flex xs)
    s <- waybelow xs' e
    return $ combine (bind x xs') s

waybelow (Tup es) (Tup es') = do
    ss <- zipWithM waybelow es es'
    return (foldl combine success ss)

-- can't go in this case. Defined just for completeness.
waybelow (Rigid p) (Rigid q) 
    | p == q    = return success -- same intensions -> same extensions
    | otherwise = fail "cannot compute if one rigid symbol is waybelow of some other rigid symbol"


-- utils

-- split a goal to an atom and the rest goal
-- deterministic computation picking always the left-most atom
-- split :: Goal a -> Infer a (Expr a, Goal a)
split []     = fail "Empty goal. Can't pick an atom"
split (x:xs) = return (x, xs)

clausesOf (App q _) = do
    p <- asks clauses
    let cl = filter (\(App r _, b)-> r == q) p
    msum (map return cl)

-- make a fresh variant of a clause.
-- monadic computation because of freshVar.
-- variant :: Clause a -> Infer a (Clause a)
-- FIXME: subst assumed to be list 
-- PITFALL: flexs are computed every time
variant c =
    let vs = vars c
        bindWithFresh v = do
            v' <- freshIt v
            return (v, Flex v')
    in do
    s <- mapM bindWithFresh vs
    return $ subst s c

freshIt :: (MonadState Int m, Functor f) => f (Symbol String) -> m (f (Symbol String))
freshIt s = do
    a' <- get
    modify (+1)
    return $ fmap (const (Sym ("_S" ++ show a'))) s
