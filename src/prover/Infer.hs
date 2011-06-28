--  Copyright (C) 2006-2011 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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
import Lang

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
-- import Data.Monoid
-- import List (last)
import Debug.Trace
import Pretty


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
derive g
  | isContra g = return (contradiction, success)
  | otherwise =
    let f g = case functor g of
                 Rigid _    -> 
                      -- trace ("Rigid resolution") $ 
                      resolveRigid g
                 Flex _     ->
                      -- trace ("Flex resolution") $ 
                      resolveFlex g
                 Lambda _ _ -> 
                      -- trace ("Beta reduce" ++ (show (ppr g))) $ 
                      lambdaReduce g
                 _ -> error  "Cannot derive anything from that atom"
    -- in undefined -- split g  >>- \(a, g') -> f g' a
    in case g of
        (App (App c a) b) ->
            if c == cand then
                derive a >>- \(g', s) ->
                    if isContra g' then
                        return (b, s)
                    else
                        return ((App (App cand g') b), s)
            else if c == cor then
                return a `mplus` return b >>- \g ->
                    derive g
            else if c == ceq then
                unify a b >>= \s ->
                return (contradiction, s)
            else if c == ctop then
                return (contradiction, success)
            else if c == cbot then
                fail "cbot"
            else f g
        _ -> f g


substFunc (App e a) b = (App (substFunc e b) a)
substFunc _ b = b

resolveRigid g =
    clauseOf (functor g) >>- \c ->
    variant c >>- \(C p b) ->
    return (substFunc g b, success)

lambdaReduce (App e a) = do
        (e',s) <- lambdaReduce e
        case e' of
            Lambda x e'' ->
                return (subst (bind x a) e'', s)
            _ -> return ((App e' a), s)
lambdaReduce e = return (e, success)

resolveFlex g =
    let f = functor g
    in case f of
          (Flex x) -> 
              singleInstance (typeOf f) >>- \fi -> do
              r <- freshVarOfType (typeOf f)
              return ((substFunc g fi), (bind x (lubExp fi (Flex r))))
          _ -> fail "resolveF: cannot resolve a non flexible"


{-
lambdaInstance ty
    | ty == tyBool = return cbot `mplus` return ctop
    | ty == tyAll  = error "cannot lambda instantiate an individual"
    | otherwise    =
        case ty of
            TyFun f a -> undefined
-}
-- Makes an instance e1 \lub e2 where e2 is a variable.
lubExp e1 e2 =
    let lubExp1 (Lambda x e) e' bs =
            (Lambda x (lubExp1 e e' (bs ++ [x])))
        lubExp1 e e' bs = (App (App cor e) e'')
            where e'' = foldl (\x -> \y -> (App x (Flex y))) e' bs
    in  lubExp1 e1 e2 []


disjLambda [] = return cbot
disjLambda (e:es) =
    case typeOf e of
        TyFun t1 t2 -> do
            x <- freshVarOfType t1
            let vs'  = map (\(Lambda v _) -> v) (e:es)
            let es'  = map (\(Lambda _ e') -> e') (e:es)
            let ss'  = zip (map (\v -> bind v (Flex x)) vs') es'
            let es'' = map (\(s,e) -> subst s e) ss'
            e' <- disjLambda es''
            return (Lambda x e')
        _ -> return $ foldl (\x -> \y -> (App (App cor x) y)) e es

comb e' (Lambda x e) = (Lambda x (comb e' e))
comb e' e = (App (App cand e') e)

basicInstance ty@(TyFun ty_arg ty_res) =
    msum (map return [1..]) >>- \n ->
    (replicateM n (singleInstance ty)) >>- \le ->
    disjLambda le
basicInstance x = singleInstance x

appInst e = do
    case typeOf e of
        TyFun t1 t2 -> do
--            a <- basicInstance t1
            a <- freshVarOfType (typeOf t1)
            appInst (App e (Flex a))
        _ -> return e

singleInstance (TyFun ty_arg ty_res) = do
    x   <- freshVarOfType ty_arg
    xe  <- case ty_arg of
             TyFun a b ->
                    msum (map return [1..]) >>- \n ->
                    (replicateM n $ appInst (Flex x)) >>- \le -> 
                    return $ foldl (\e -> \e2 -> (App (App cand e) e2)) (head le) (tail le)
             _ -> if ty_arg == tyAll then do
                        y <- singleInstance ty_arg
                        return (App (App ceq (Flex x)) y)
                  else if ty_arg == tyBool then do
                        y <- singleInstance ty_arg
                        return $ if y == ctop then (Flex x) else y
                  else
                        fail ""
    if (ty_res == tyBool)
       then return (Lambda x xe)
       else singleInstance ty_res >>- \res -> return (Lambda x (comb xe res))
--    res <- singleInstance ty_res
--    return $ if (ty_res == tyBool) then (Lambda x xe) else (Lambda x (comb xe res))

singleInstance ty
    | ty == tyBool = return cbot `mplus` return ctop
    | ty == tyAll  = freshVarOfType ty >>= \x -> return (Flex x)
    | otherwise = fail "cannot instantiate from type"

-- unification

unify :: (Symbol a, Eq a, Monad m) => Expr a -> Expr a -> m (Subst a)

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

-- unify (Tup es) (Tup es') = listUnify es es'

unify (Rigid p) (Rigid q)
    | p == q    = return success
    | otherwise = fail "Unification fail"

unify _ _ = fail "Should not happen"

occurCheck :: (Symbol a, Eq a, Monad m) => a -> Expr a -> m ()
occurCheck a e = when (a `occursIn` e) $ fail "Occur Check"

occursIn a e = a `elem` (vars e)

-- utils

-- split a goal to an atom and the rest goal
-- deterministic computation picking always the left-most atom
-- split :: Goal a -> Infer a (Expr a, Goal a)
split []     = fail "Empty goal. Can't pick an atom"
split (x:xs) = return (x, xs)

clauseOf (Rigid r) = do
    p <- asks clauses
    let cl = filter (\(C p' _) -> p' == r) p
    msum (map return cl)
clauseOf e = fail "expression must be rigid (parameter)"

variant c =
    let vs = vars c
        bindWithFresh v = do
            v' <- freshVarOfType (typeOf v)
            return (v, Flex v')
    in do
    s <- mapM bindWithFresh vs
    return $ subst s c

freshVarOfType :: (MonadState Int m, Symbol a, HasType a) => Type -> m a
freshVarOfType ty = do
    a' <- get
    modify (+1)
    return $ hasType ty $ liftSym ("_S" ++ show a')
