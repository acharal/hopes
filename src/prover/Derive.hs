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
import Subst (subst, bind, success, dom)

import CoreLang (Expr(..), fv, functor, args, isVar, splitExist, exists)
import Types (MonoTypeV(..), tyBool, tyAll, typeOf)

import Control.Monad (msum, mplus, replicateM)
import Data.List (partition)

import Infer.Class

splitAnd (And e1 e2) = splitAnd e1 ++ splitAnd e2
splitAnd e = [e]

foldAnd es = foldr1 And es

isUnsatisf vs theta = not satisf
    where satisf = norenames && not (null onlyfree)
          onlyfree = filter (\(v,_) -> v `notElem` vs) theta
          norenames = all (\(v,Var x) -> x `notElem` vs) $ filter (\(v,t) -> isVar t) onlyfree

reduce (Not e) = 
    let (vs, e') = splitExist e
        es = splitAnd e'
        aux vs (Eq e1 e2) [] []       = ifte (unify e1 e2) nonvalid valid
            where valid = return CTrue
                  nonvalid theta | isUnsatisf vs theta = return CFalse 
                                 | otherwise           = fail "satisfiable"

        aux vs eq@(Eq _ _) rest diseq  = 
            let (vs1, vs2) = partition (\v -> v `elem` (fv eq)) vs
            in return $ Or (Not (exists vs1 eq)) 
                           (exists vs1 (And eq (aux2 vs2 (rest ++ diseq))))

        aux vs (Or e1 e2) rs diseq = 
            return $ And (aux2 vs ((e1:rs) ++ diseq))
                         (aux2 vs ((e2:rs) ++ diseq))

        aux vs (Exists x e) re diseq = do
            x' <- freshVarOfType (typeOf x)
            let e' = subst (bind x (Var x')) e
            return $ aux2 (x':vs) ((e':re) ++ diseq)

        aux vs (Not e) [] [] = 
            if all (\v -> v `notElem` vs) (fv e)
            then return e
            else aux3 vs (Not e)

        aux vs e rest diseq 
            | isVarApp e = aux4 vs e (rest ++ diseq)
            | otherwise = ifte (reduce e) s f
            where s CFalse = return CTrue
                  s CTrue  = return $ aux2 vs $ rest ++ diseq
                  s e'     = return $ aux2 vs $ (e':rest) ++ diseq
                  f = if null rest 
                      then dis vs (e:diseq)
                      else aux vs (head rest) (tail rest) (e:diseq)

        aux2 vs [] = CFalse
        aux2 vs es = Not $ exists vs $ foldAnd es 

        dis vs es = return $ foldr1 Or $ map (\e -> Not (exists vs e)) es

        isVarApp (Not e) = isVarApp e'
            where (_, e') = splitExist e
        isVarApp e = isVar $ functor e
        isDisequal (Not e) = isEq e'
            where (_, e') = splitExist e
                  isEq (Eq _ _) = True
                  isEq _ = False
        isDisequal _ = False

        aux3 vs e | isDisequal e = return CFalse
                  | isVarApp e   = aux4 vs e []

        aux4 vs e [] = undefined
        aux4 vs e rs =
            let (vs1, vs2) = partition (\v -> v `elem` (fv e)) vs
            in return $ Or (Not (exists vs1 e))
                           (And (exists vs1 (And e (aux2 vs2 rs)))
                                (aux2 vs (e:rs)) 
                           )

    in aux vs (head es) (tail es) []

reduce e = 
    case functor e of 
        Rigid _    -> resolveRigid e
        Lambda _ _ -> lambdaReduce e
        And _ _    -> expandApp e
        Or  _ _    -> expandApp e
        _ -> fail "cannot reduce"

expandApp (App (And e1 e2) e) = return $ And (App e1 e) (App e2 e)
expandApp (App (Or  e1 e2) e) = return $ Or  (App e1 e) (App e2 e)
expandApp (App e1 e)          = do
    e1' <- (expandApp e1)
    expandApp (App e1' e)
expandApp e                   = fail ("expandApp")

returnSuccess e = return (e, success)

derive CFalse = fail "false"

derive e =
    let es = splitAnd e
        aux (Eq e1 e2) es disequal = do
            theta <- unify e1 e2
            return (subst theta $ aux3 (es ++ disequal), theta)
        aux (Or e1 e2) es disequal = mplus r1 r2
            where r1 = returnSuccess $ aux2 e1 (es ++ disequal)
                  r2 = returnSuccess $ aux2 e2 (es ++ disequal)
        aux (Exists x e) es disequal = do 
            x' <- freshVarOfType (typeOf x)
            returnSuccess $ aux2 (subst (bind x (Var x')) e) (es ++ disequal)

        aux e es disequal | isVarApp' e = do 
                                (g, s) <- resolveFlex' [] e
                                return (subst s $ aux3 (es++disequal) , s)
                          | otherwise = ifte (reduce e) s f
            where s CTrue  = returnSuccess $ aux3 (es ++ disequal)
                  s CFalse = returnSuccess CFalse
                  s e' = returnSuccess $ aux2 e' (es ++ disequal)
                  f = if null es 
                      then fail "all disequalities"
                      else aux (head es) (tail es) (e:disequal)

        aux2 e es = foldAnd (e:es)
        aux3 [] = CTrue
        aux3 (e:es) = aux2 e es

        isVarApp' (Not e) = isVarApp'' e'
            where (_, e') = splitExist e
        isVarApp' e = isVarApp'' e

        isVarApp'' (Not e) = isVar (functor e)
        isVarApp'' e = isVar (functor e)

        resolveFlex' [] (Not e) = resolveFlexNot' vs e'
            where (vs, e') = splitExist e
        resolveFlex' [] e = resolveFlex [] e
        resolveFlexNot' vs (Not e) = resolveFlex vs e
        resolveFlexNot' vs e = resolveFlexNot vs e

    in aux (head es) (tail es) []


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

resolveFlexNot vs g =  
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

resolveFlex vs g =
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

{-
posSingleInstance ty = 
    let pos (Lambda x e) = Lambda x (pos e)
        pos e = (exists vs e)
    in do
        i <- singleInstance ty  
        return $ pos i

negSingleInstance ty@(TyFun _ _) = 
    let neg (Lambda x e) = Lambda x (neg e)
        neg e = Not (exists vs e)
    in do
        i <- singleInstance ty
        return $ neg i
negSingleInstance vs ty = singleInstance ty
-}

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
