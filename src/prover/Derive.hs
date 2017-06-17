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
import Subst (subst, bind, success, dom, combine)

import CoreLang (Expr(..), fv, functor, args, isVar, splitExist, exists)
import Types (MonoTypeV(..), tyBool, tyAll, typeOf)

import Control.Monad (msum, mplus, replicateM)
import Data.List (partition)

import Infer.Class

-- utils

splitAnd (And e1 e2) = splitAnd e1 ++ splitAnd e2
splitAnd e = [e]

foldAnd [] = CTrue
foldAnd es = foldr1 And es
foldOr  es = foldr1 Or es

isUnsatisf vs theta = not satisf
    where satisf = norenames && not (null onlyfree)
          onlyfree = filter (\(v,_) -> v `notElem` vs) theta
          norenames = all (\(v,Var x) -> x `notElem` vs) $ filter (\(v,t) -> isVar t) onlyfree

notExists vs [] = CFalse
notExists vs es  = Not $ exists vs $ foldAnd es

notEq vs e1 e2 = notExists vs [(Eq e1 e2)]

isDisEq (Not e) = isEq e'
    where (_, e') = splitExist e
isDisEq _ = False

isEq (Eq _ _) = True
isEq _ = False

isPrimDisEq (Not e) = isPrimEq e'
    where (_, e') = splitExist e
          isPrimEq (Eq e1 e2) = isVar e1 || isVar e2
          isPrimEq _ = False
isPrimDisEq _ = False

renameSubst = zipWith vv
    where vv v1 v2 = (v1, Var v2)

renameM vs e = do
    vs' <- mapM (freshVarOfType.typeOf) vs
    let e' = subst (renameSubst vs vs') e
    return (e', vs')

negreduce (Not e) =
    let (u, e') = splitExist e
        (t:ts)  = splitAnd e'

        negreduce' CTrue  es =
            case es of
                [] -> return CFalse
                _  -> return $ notExists u es

        negreduce' CFalse es = return CTrue

        negreduce' e@(Exists _ _) es = do
                (e2, xs') <- renameM xs e'
                return $ notExists (u ++ xs') (e2:es)
            where (xs, e') = splitExist e

        negreduce' (Or e1 e2) es = return $ And (b e1) (b e2)
            where b e = notExists u (e:es)

        {- equality -}
        negreduce' (Eq e1 e2) es = ifte (unify e1 e2) nonvalid valid
            where diseq = notExists u [(Eq e1 e2)]
                  valid = return CTrue

                  nonvalid theta
                        | isPrimDisEq diseq &&
                          isUnsatisf u theta      = unsatisf theta es
                        | isPrimDisEq diseq &&
                          null es                 = fail "prim disequality"
                        | isPrimDisEq diseq       = chan u (Eq e1 e2) es
                        | otherwise               = unfold theta es

                  unfold theta es = return $ notExists u (x:es)
                        where x = foldAnd $ map (eq theta) $ dom theta
                              eq theta x = Eq (Var x) (subst theta (Var x))

                  unsatisf [] es = return $ notExists u es
                  unsatisf theta@[(x, e)] es =
                        return $ notExists u $ map (subst theta') es
                    where theta' | x `elem` u  = theta
                                 | otherwise   = case e of
                                                     Var y -> bind y (Var x)
                                                     _ -> fail "not unsatisf"
                  chan vs eq@(Eq _ _) es =
                        return $ Or (Not (exists vs1 eq))
                                 (exists vs1 (And eq (notExists vs2 es)))
                     where (vs1, vs2) = partition (\v -> v `elem` (fv eq)) vs


        negreduce' e@(Not ee) es = ifte (negreduce e) reducable rest
            where reducable e' = return $ notExists u (e':es)
                  rest | isVarApp e    = handleHigher u e es
                       | isPrimDisEq e = handleDisequal
                       | otherwise     = error "WTF"
                  handleDisequal | all (`notElem` u) (fv e) = return $ Or ee (notExists u es)
                                 | all isPrimDisEq es  = return $ notExists u es
                                 | otherwise = fail "handleDisequal"

        negreduce' e es
            | isVarApp e = handleHigher u e es
            | otherwise  = do
                    e' <- reduce e
                    return $ notExists u (e':es)

        negreduceSeq e es d = ifte (negreduce' e (es ++ d)) return f
            where f | not (null es) = negreduceSeq (head es) (tail es) (e:d)
                    | otherwise     = fail "not reducable"

        handleHigher vs e es =
            let args' (Not e) = args' e'
                    where (_, e') = splitExist e
                args' e = args e
                functor' (Not e) = functor' e'
                    where (_, e') = splitExist e
                functor' e = functor e

                aux | (functor' e) `isIn` vs = aux1   {-  Def 22.[7,10,11].(a) -}
                    | not (null es)          = aux2   {-  Def 22.[7,10,11].(b) -}
                    | otherwise              = aux3 e {-  Def 22.[10,11].(c)   -}

                isIn (Var v) vs = v `elem` vs


                {-  Def 22.[7,10,11].(a) -}
                aux1 = do
                    (_, s) <- resolveFlex e
                    return $ notExists u $ map (subst s) es {- must include the new var R' to "u" -}

                {-  Def 22.[7,10,11].(b) -}
                aux2 =
                    let (vs1, vs2) = partition (\v -> v `elem` (fv e)) vs
                    in do
                      vs1' <- mapM (freshVarOfType.typeOf) vs1
                      vs2' <- mapM (freshVarOfType.typeOf) vs2
                      let vs' = vs1' ++ vs2'
                      let (Var r) = functor e
                      let renaming = (renameSubst vs1 vs1') `combine` (renameSubst vs2 vs2') `combine` [(r, (Select (functor e)))]
                      let e' = subst (renaming) e
                      let es' = map (subst renaming) es
                      let eq = map (\(v,e) -> Eq (Var v) e) renaming
                      let es'' = foldAnd eq `Or` foldAnd es'
                      return $ notExists vs1 [e] `Or`
                                  (exists vs1 (e `And` (notExists vs2 es) `And` (notExists vs' ([e', es'']))))

                {-  Def 22.[10,11].(c)
                    Note: This is always of form (Not e). The other cases are handled by derive Def 20.6.
                -}
                aux3 (Not e) =
                    let (vs, e') = splitExist e
                    in if null vs
                       then fail "not reducable"
                       else do
                            (e'', vs') <- renameM vs e'
                            return $ exists vs $ notExists u $ [Not e', notExists vs' [e'']]
                aux3 _ = fail "not reducable"

            in aux

        isVarApp (Not e) = isVarApp e'
            where (_, e') = splitExist e
        isVarApp e = isVar $ functor e

    in negreduceSeq t ts []

negreduce _ = fail "not a negative expression"

reduce e =
    case functor e of
        Rigid _    -> resolveRigid e
        Lambda _ _ -> lambdaReduce e
        And _ _    -> expandApp e
        Or  _ _    -> expandApp e
        Select f   -> return $ substFunc (select f) e
        _ -> fail "cannot reduce"

-- select :: Expr a -> Expr a
select (Or e1 e2) = e2
select (And e1 e2) = e2
select e = error $ "Ergys definition is partial and failed on expression " ++ (show e)

expandApp (App (And e1 e2) e) = return $ And (App e1 e) (App e2 e)
expandApp (App (Or  e1 e2) e) = return $ Or  (App e1 e) (App e2 e)
expandApp (App e1 e)          = do
    e1' <- (expandApp e1)
    expandApp (App e1' e)
expandApp e                   = fail ("expandApp")

returnSuccess e = return (e, success)

derive CFalse = fail "false"

derive e =
    let deriveSingle (Eq e1 e2) = do
            theta <- unify e1 e2
            return (CTrue, theta)

        deriveSingle (Or e1 e2) = mplus (b e1) (b e2)
            where b = returnSuccess

        deriveSingle e@(Exists _ _) = do
            (e'', _) <- renameM vs e'
            returnSuccess e''
           where (vs, e') = splitExist e

        deriveSingle e@(Not _) | isVarApp' e = do
                            (e', s) <- resolveFlex e
                            return (CTrue, s)
                               | otherwise =  negreduce e >>= returnSuccess

        deriveSingle e | isVarApp' e = do
                            (e', s) <- resolveFlex e
                            return (CTrue, s)
                       | otherwise   = reduce e >>= returnSuccess

        deriveSeq e es d = ifte (deriveSingle e) succ f
            where succ (CFalse, ss) = return (CFalse, ss)
                  succ (CTrue, ss)  = return (subst ss (foldAnd (d ++ es)),   ss)
                  succ (e', ss) = return (subst ss (foldAnd (e':(d ++ es))), ss)
                  f | null es   = fail "all disequalities"
                    | isPrimDisEq e = deriveSeq (head es) (tail es) (e:d)
                    | otherwise = fail ""

        isVarApp' (Not e) = isVarApp'' e'
            where (_, e') = splitExist e
        isVarApp' e = isVarApp'' e

        isVarApp'' (Not e) = isVar (functor e)
        isVarApp'' e = isVar (functor e)

        es = splitAnd e

    in deriveSeq (head es) (tail es) []


substFunc (App e a) b = (App (substFunc e b) a)
substFunc _ b = b

resolveRigid g =
    let lam e (TyFun a b) = do
            l <- lam e b
            v <- freshVarOfType a
            return (Lambda v l)
        lam e ty = return e
        body [] = lam CFalse (typeOf (functor g))
        body bs = return $ foldl1 Or bs
        clauses (Rigid r) = clausesOf r
    in do
       es <- clauses (functor g)
       e  <- body es
       return $ substFunc g e

lambdaReduce = betaReduce

alphaConvert (Lambda x e) = do
    x' <- freshVarOfType (typeOf x)
    e' <- alphaConvert e
    return $ Lambda x' (subst (bind x (Var x')) e')
alphaConvert (Exists x e) = do
    x' <- freshVarOfType (typeOf x)
    e' <- alphaConvert e
    return $ Exists x' (subst (bind x (Var x')) e')
alphaConvert e = return e

betaReduce (App e a) = do
    e' <- betaReduce e
    case e' of
        Lambda x e'' ->
           return $ subst (bind x a) e''
        _ ->
           return $ (App e' a)
betaReduce e = alphaConvert e


resolveFlex (Not e) = resolveFlexNot' vs e'
    where (vs, e') = splitExist e
          resolveFlexNot' vs (Not e) = resolveFlexPos vs e
          resolveFlexNot' vs e       = resolveFlexNeg vs e
resolveFlex e = resolveFlexPos [] e

resolveFlexNeg vs g =
    let neg vs (Lambda x e) = Lambda x (neg vs e)
        neg vs e = Not (exists vs' e')
            where vs'  = map snd vs
                  vs'' = map (\(x,y) -> (x, Var y)) vs
                  e'   = subst vs'' e
    in case functor g of
           Var x ->
               singleInstance (typeOf x) >>- \fi -> do
               fi' <- instantiate fi g
               r <- freshVarOfType (typeOf x)
               vs' <- mapM (\x -> freshVarOfType (typeOf x) >>= \v -> return (x, v)) vs
               let b  = neg vs' fi'
               let s  = bind x (And b (Var r))
               let g' = subst s (substFunc g b)
               return (g', s)

resolveFlexPos vs g =
    let pos vs (Lambda x e) = Lambda x (pos vs e)
        pos vs e = (exists  vs' e')
            where vs'  = map snd vs
                  vs'' = map (\(x,y) -> (x, Var y)) vs
                  e'   = subst vs'' e
    in case functor g of
           Var x ->
               singleInstance (typeOf x) >>- \fi -> do
               fi' <- instantiate fi g
               r <- freshVarOfType (typeOf x)
               vs' <- mapM (\x -> freshVarOfType (typeOf x) >>= \v -> return (x, v)) vs
               let b  = pos vs' fi'
               let s  = bind x (Or b (Var r))
               let g' = subst s (substFunc g b)
               return (g', s)
           _ -> fail "resolveF: cannot resolve a non flexible"


instantiate f g =
    let l (Lambda x e) = x:(l e)
        l _ = []
        sl = zip (l f) (args g)
        s (Eq (Var v) e) =
            case lookup v sl of
                Just e' -> (Eq (Var v) e')
                Nothing -> (Eq (Var v) e)
        s (And e1 e2) = And (s e1) (s e2)
        s (Lambda x e) = Lambda x (s e)
        s e = e
    in return (s f)


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
