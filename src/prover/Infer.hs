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

-- | Proof procedure of Hopl
module Infer (runInfer, infer, prove) where

-- import Language.Hopl
import Subst
import Logic
import Types (Type(..), MonoTypeV(..), tyBool, tyAll, HasType(..), hasType)
-- import Lang (cor, cand, ceq, ctop, cbot, vars, liftSym, Symbol, contradiction, isContra)
import Lang (liftSym, Symbol)

import CoreLang

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
-- import Data.Monoid
-- import List (last)
import Debug.Trace

type Infer a = ReaderT (Program a) (StateT Int (LogicT Identity))

runInfer p m = runIdentity $ runLogic Nothing $ evalStateT (runReaderT m p) 0

infer :: Program a -> Infer a b -> Maybe (b, Infer a b)
infer p m =  runIdentity $ observe $ evalStateT (runReaderT (msplit m) p) 0

-- try prove a formula by refutation
-- prove  :: Goal a -> Infer a (Subst a)
prove g =  do
    ans <- refute g
    return (restrict (fv g) ans)

-- do a refutation
-- refute :: Goal a -> Infer a (Subst a)
refute g
    | g == CTrue = return success
    | otherwise  = derive g >>- \(g',  s)  ->
                   refute (subst s g') >>- \ans ->
                   return (s `combine` ans)

-- single step derivation
-- derive :: Goal a -> Infer a (Goal a, Subst a)

derive (Eq  e1 e2)   = do
    mgu <- unify e1 e2
    return (CTrue, mgu)

derive (And CTrue e) = return (e, success)
derive (And e CTrue) = return (e, success)
derive (And e1 e2)   = do
    (e1',theta) <- derive e1
    return ((And e1' (subst theta e2)), theta)

derive (Or  e1 e2)   = mplus (return (e1, success)) (return (e2, success))
derive (Exists v e)  = do
    v' <- freshVarOfType (typeOf v)
    return (subst (bind v (Var v')) e, success)

derive e = case functor e of
            Rigid _    -> resolveRigid e
            Var _      -> resolveFlex  e
            Lambda _ _ -> lambdaReduce e
            _          -> return (expandApp e, success)
    where expandApp (App (And e1 e2) e) = And (App e1 e) (App e2 e)
          expandApp (App (Or  e1 e2) e) = Or  (App e1 e) (App e2 e)
          expandApp (App e1 e)          = expandApp (App (expandApp e1) e)
          expandApp e                   = error ("expandApp" ++ show e)

substFunc (App e a) b = (App (substFunc e b) a)
substFunc _ b = b

resolveRigid g = do
    e <- clauseOf (functor g)
    return (substFunc g e, success)

lambdaReduce = betaReduce

alphaConvert (Lambda x e) = do
    x' <- freshVarOfType (typeOf x)
    e' <- alphaConvert e
    return $ Lambda x' (subst (bind x (Var x')) e')
alphaConvert (Exists x e) = do
    x' <- freshVarOfType (typeOf x)
    e' <- alphaConvert e
    return $ Exists x' (subst (bind x (Var x')) e')
alphaConvert (Forall x e) = do
    x' <- freshVarOfType (typeOf x)
    e' <- alphaConvert e
    return $ Forall x' (subst (bind x (Var x')) e')
alphaConvert e = return e

betaReduce (App e a) = do
        (e',s) <- betaReduce e
        case e' of
            Lambda x e'' ->
                return (subst (bind x a) e'', s)
            _ -> return ((App e' a), s)
betaReduce e = do
    e' <- alphaConvert e
    return (e', success)

resolveFlex g =
    let f = functor g
    in case f of
          Var x -> 
              singleInstance (typeOf x) >>- \fi -> do
              r <- freshVarOfType (typeOf x)
              return ((substFunc g fi), (bind x (lubExp fi (Var r))))
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
{-
lubExp e1 e2 =
    let lubExp1 (Lambda x e) e' bs =
            (Lambda x (lubExp1 e e' (bs ++ [x])))
        lubExp1 e e' bs = (App (App cor e) e'')
            where e'' = foldl (\x -> \y -> (App x (Flex y))) e' bs
    in  lubExp1 e1 e2 []
-}
lubExp e1 e2  = disjLambda [e1,e2]
disjLambda es = foldl1 Or es

{-
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
-}


basicInstance ty@(TyFun _ _) =
    msum (map return [1..])          >>- \n ->
    replicateM n (singleInstance ty) >>- \le ->
    return $ disjLambda le
basicInstance x = singleInstance x


singleInstance (TyFun ty_arg ty_res) = 
    let argExpr (TyFun _ _) x       = msum (map return [1..])        >>- \n -> 
                                      replicateM n (appInst (Var x)) >>- \le ->
                                      return $ foldl1 And le
        argExpr ty x | ty == tyAll  = do { y <- singleInstance ty
                                         ; return $ Eq (Var x) y
                                         }
                     | ty == tyBool = do { y <- singleInstance ty
                                         ; return $ if y == CTrue then (Var x) else y
                                         }
                     | otherwise    = fail ""
        comb e' (Lambda x e) = (Lambda x (comb e' e))
        comb e' e = And e' e
        appInst e = do
             case typeOf e of
                 TyFun t1 _ -> do
                      a <- freshVarOfType (typeOf t1)
                      appInst (App e (Var a))
                 _ -> return e
    in do
       x   <- freshVarOfType ty_arg
       xe  <- argExpr ty_arg x

       if (ty_res == tyBool)
        then return (Lambda x xe)
        else singleInstance ty_res >>- \res -> 
             return (Lambda x (comb xe res))

singleInstance ty
    | ty == tyBool = return CFalse `mplus` return CTrue
    | ty == tyAll  = freshVarOfType ty >>= \x -> return (Var x)
    | otherwise    = fail "cannot instantiate from type"

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

unify _ _ = fail "Should not happen"

-- occurCheck :: (Symbol a, Eq a, Monad m) => a -> Expr a -> m ()
occurCheck a e = when (a `occursIn` e) $ fail "Occur Check"

occursIn a e = a `elem` (fv e)

-- utils

-- split a goal to an atom and the rest goal
-- deterministic computation picking always the left-most atom
-- split :: Goal a -> Infer a (Expr a, Goal a)
split []     = fail "Empty goal. Can't pick an atom"
split (x:xs) = return (x, xs)

clauseOf (Rigid r) = do
    cl <- asks (clausesOf r)
    msum (map return cl)
clauseOf e = fail "expression must be rigid (parameter)"

{- 
variant c =
    let vs = vars c
        bindWithFresh v = do
            v' <- freshVarOfType (typeOf v)
            return (v, Flex v')
    in do
    s <- mapM bindWithFresh vs
    return $ subst s c
-}

freshVarOfType :: (MonadState Int m, Symbol a, HasType a) => Type -> m a
freshVarOfType ty = do
    a' <- get
    modify (+1)
    return $ hasType ty $ liftSym ("V" ++ show a')
