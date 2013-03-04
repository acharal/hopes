--  Copyright (C) 2006-2013 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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
module Derive (derive) where

import Unify (unify)
import Subst (subst, bind, success)

import CoreLang (Expr(..), functor)
import Types (MonoTypeV(..), tyBool, tyAll, typeOf)

import Control.Monad (msum, mplus, replicateM)

import Infer.Class

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

derive (CFalse) = fail ""
derive (CTrue)  = return (CTrue, success)

derive e =
    case functor e of
       Rigid _    -> resolveRigid e
       Var _      -> resolveFlex  e
       Lambda _ _ -> lambdaReduce e
       And _ _    -> return (expandApp e, success)
       Or  _ _    -> return (expandApp e, success)
       _          -> fail "No derivation for that expression"
    where expandApp (App (And e1 e2) e) = And (App e1 e) (App e2 e)
          expandApp (App (Or  e1 e2) e) = Or  (App e1 e) (App e2 e)
          expandApp (App e1 e)          = expandApp (App (expandApp e1) e)
          expandApp e                   = error "expandApp"

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
        _ ->
           return ((App e' a), s)
betaReduce e = do
    e' <- alphaConvert e
    return (e', success)

resolveFlex g =
    case functor g of
        Var x -> 
            singleInstance (typeOf x) >>- \fi -> do
            r <- freshVarOfType (typeOf x)
            let s  = bind x (Or fi (Var r))
            let g' = subst s (substFunc g fi)
            return (g', s)
        _ -> fail "resolveF: cannot resolve a non flexible"

basicInstance ty@(TyFun _ _) =
    msum (map return [1..])          >>- \n ->
    replicateM n (singleInstance ty) >>- \le ->
    return $ foldl1 Or le
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
        comb e' e            = And e' e
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

-- utils

clauseOf (Rigid r) = do
    cl <- clausesOf r
    msum (map return cl)
clauseOf e = fail "expression must be rigid (parameter)"
