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
import Subst (subst, bind, combine, success, dom, vars, isRenamingSubst)

import CoreLang (Expr(..), fv, functor, args, isVar, splitExist, exists)
import Types (MonoTypeV(..), tyBool, tyAll, typeOf)

import Control.Monad (msum, mplus, replicateM)
import Data.List (partition)

import Infer.Class


reduce (Not (Not e)) = return e
reduce (Not CFalse)  = return CTrue
reduce (Not CTrue)   = return CFalse
reduce (Not (And e1 e2)) = return (Or (Not e1) (Not e2))
reduce (Not (Or e1 e2))  = return (And (Not e1) (Not e2))
reduce (Not e) = do
    e' <- reduce e 
    return (Not e')

reduce (And CTrue e) = return e
reduce (And e CTrue) = return e
reduce (And CFalse e) = return CFalse
reduce (And e CFalse) = return CFalse
reduce (And (And e1 e2) e3) = return (And e1 (And e2 e3))
reduce (And (Or e1 e2) e3)  = return (Or (And e1 e3) (And e2 e3))
reduce (And e1 e2) = ifte r1 return r2
    where r1 = reduce e1 >>= \e1' -> return (And e1' e2)
          r2 = reduce e2 >>= \e2' -> return (And e1 e2')

reduce (Exists v (Or e1 e2)) = return (Or (Exists v e1) (Exists v e2))
reduce (Exists v e) = 
    if v `notElem` (fv e)
    then return e
    else do
        e' <- reduce e
        return (Exists v e')

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

unifyD e = uncurry unify' $ splitExist e

unify' vs (Eq e1 e2) = ifte (unify e1 e2) nonvalid valid
    where isSatisfiable theta = any (\x -> x `notElem` vs) $ dom theta
          valid = returnSuccess CFalse
          nonvalid mgu | isSatisfiable mgu = satisfy mgu
                       | otherwise = returnSuccess CTrue
          satisfy mgu = return (CTrue, mgu')
             where mgu' = filter (\(v,_) -> v `notElem` vs) mgu 
unify' _ _ = fail "Not equality"

reduceD e = reduce e >>= returnSuccess

-- single step derivation
-- derive :: Goal a -> Infer a (Goal a, Subst a)
{-
derive (CFalse) = fail ""
derive (CTrue)  = returnSuccess CTrue
-}
derive (Cut)    = returnSuccess CTrue -- cut >> return (CTrue, success)

derive e@(Eq _ _) = unifyD e

{-
derive (And CTrue e) = returnSuccess e
derive (And e CTrue) = returnSuccess e
-}
derive e@(And CTrue _)  = reduceD e
derive e@(And _ CTrue)  = reduceD e
derive e@(And CFalse _) = reduceD e
derive e@(And _ CFalse) = reduceD e
derive e@(And _ _) = ifte (derive' e) return (reduceD e)
    where derive' e = ifte (derive1 e) return (derive2 e)
          derive1 e@(And e1 e2) = do
            (e1',theta) <- derive e1
            return (And e1' (subst theta e2), theta)
          derive2 e@(And e1 e2) = do
            (e2',theta) <- derive e2
            return (And (subst theta e1) e2', theta)

derive (Or  e1 e2)   = mplus (returnSuccess e1) (returnSuccess e2)

{-
derive (Not CTrue)   = fail ""
derive (Not CFalse)  = returnSuccess CTrue

derive e@(Not (Not _)) = reduce e >>= returnSuccess
-}
derive ne@(Not (Not _)) = reduceD ne
derive ne@(Not e) = ifte neg return (reduceD ne)
    where neg = do
            (e', s) <- neg_derive e
            return (Not e', s)

derive (Exists v e)  = do
    v' <- freshVarOfType (typeOf v)
    returnSuccess $ subst (bind v (Var v')) e

derive e = 
    case functor e of 
        Var _ -> resolveFlex [] e
        _     -> reduceD e


neg_derive e = uncurry neg_derive' $ splitExist e

negneg_derive e = uncurry negneg_derive' $ splitExist e
-- negneg_derive = derive 

negneg_derive' vs e@(Eq _ _) = unify' vs e >>= pos
    where pos (CTrue, mgu) = do
                rtheta <- mapM fresh vs
                return (CTrue, mgu `combine` rtheta)
          pos (CFalse, s)  = return (CFalse, s)
          fresh v = do
              x <- freshVarOfType (typeOf v)
              return (v, Var x)

-- negneg_derive' vs (Not e) = do
--      (e', s) <- neg_derive e

negneg_derive' vs e@(And e1@(Eq _ _) e2) =
    let fv1 = fv e1
        (vs1, vs2) = partition (\v -> v `elem` fv1) vs
        ne e1 = Not (exists vs1 (And e1 (Not (exists vs2 e2))))
    in returnSuccess $ And (exists vs1 e1) (ne e1)

negneg_derive' vs e = 
    case functor e of
        Var x -> 
            if x `elem` vs 
            then returnSuccess CFalse
            else do
                (e', s) <- resolveFlex vs e
                return (CTrue, s)
        _ -> fail ""


neg_derive' vs e@(Eq e1 e2) = unify' vs e >>= neg
    where equality mgu x = returnSuccess $ exists vs $ Eq (Var x) (subst mgu (Var x))
          eqExp mgu = msum $ map (equality mgu) $ dom mgu
          neg (CTrue, [])  = return (CTrue, [])
          neg (CTrue, mgu) | isVar e1  = fail "satisfiable" 
                           | otherwise = eqExp mgu
          neg (CFalse, s)  = return (CFalse, s)

neg_derive' vs (Not e) = 
    let isValid' vs s = isRenamingSubst $ filter (aux vs) s
            where aux vs (v, t) = all (\v -> v `notElem` fv t) vs && all (\x -> x /= v) vs
        isValid vs s = all (\x -> x `notElem` vs) $ vars s
    in case functor e of
        Var x -> 
            if x `elem` vs
            then returnSuccess CTrue
            else do
                (e', s) <- resolveFlex vs e
                return (CFalse, s)
                -- return (exists vs (Not e'), s)
        _ -> do
            (e', s) <- negneg_derive e
            if not (isValid vs s)
             then returnSuccess CTrue
             else returnSuccess (subst s (exists vs (Not e')))

neg_derive' vs e@(And e1 e2) = 
    let fv1 = fv e1
        (vs1, vs2) = partition (\v -> v `elem` fv1) vs
        ne e1 = Not (exists vs1 (And e1 (Not (exists vs2 e2))))
        neg_derive'' = 
            case functor e1 of 
                Eq _ _ -> returnSuccess (ne e1)
                Var x  -> 
                    if x `elem` vs
                    then returnSuccess (ne e1)
                    else resolveFlex' e1 ne (\e -> exists vs (And e e2))
                _ -> fail ""
        resolveFlex' g n m = do
             case functor g of
                Var x -> 
                    singleInstance (typeOf x) >>- \fi -> do
--                    b <- instantiate fi g
                    let b = fi
                    r <- freshVarOfType (typeOf x)
                    let s  = bind x (Or b (Var r))
                    let g' = subst s (substFunc g b)
                    let r' = substFunc g (Var r)
                    return (subst s (Or (n g') (m r')), s)
                _ -> fail "resolveF: cannot resolve a non flexible"
    in case functor e1 of 
            Eq _ _ -> 
                mplus (returnSuccess (exists vs e1)) neg_derive'' 
            Var _ -> 
                mplus (returnSuccess (exists vs e1)) neg_derive'' 
            Not (Exists _ _) ->
                mplus (returnSuccess (exists vs e1)) (returnSuccess (exists vs e2))
            _ -> fail ""


neg_derive' vs e = 
    case functor e of
        Var x -> 
            if x `elem` vs 
            then returnSuccess CTrue
            else do
                (e', s) <- resolveFlexNot vs e
                return (CFalse, s)
                -- return (exists vs e', s)
        _ -> fail ""


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
