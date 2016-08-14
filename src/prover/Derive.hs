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
import Subst (subst, bind, success, dom, Subst)

import Core (CExpr(..), fv, functor, args, isVar, splitExist, exists, Flex(..), isNamedCVar, elemVar)
import Types hiding (tyBool, tyAll)-- (MonoTypeV(..), tyBool, tyAll, typeOf)

import Control.Monad (msum, mplus, replicateM)
import Data.List (partition)
import Data.Maybe (catMaybes)

import Infer.Class
import Pretty

tyBool = Rho_pi Pi_o
tyAll  = Rho_i

-- utils

splitAnd (CAnd _ e1 e2) = splitAnd e1 ++ splitAnd e2
splitAnd e = [e]

foldAnd [] = CTrue
foldAnd es = foldr1 (CAnd (Rho_pi Pi_o)) es
foldOr  es = foldr1 (COr  (Rho_pi Pi_o)) es

isUnsatisf :: [Flex] -> Subst -> Bool
isUnsatisf vs theta = not satisf
    where satisf = norenames && not (null onlyfree)
          onlyfree = filter (\(v,_) -> v `notElem` vs) theta
          norenames = all (\(v,CVar x) -> x `notElem` vs) $ filter (\(v,t) -> isVar t) onlyfree

notExists vs [] = CFail
notExists vs es  = CNot $ exists vs $ foldAnd es

notEq vs e1 e2 = notExists vs [(CEq e1 e2)]

isDisEq (CNot e) = isEq e'
    where (_, e') = splitExist e
isDisEq _ = False

isEq (CEq _ _) = True
isEq _ = False

isPrimDisEq (CNot e) = isPrimEq e'
    where (_, e') = splitExist e
          isPrimEq (CEq e1 e2) = isVar e1 || isVar e2
          isPrimEq _ = False
isPrimDisEq _ = False

renameSubst s1 s2 = catMaybes $ zipWith vv s1 s2
    where vv v1@(Flex _ _) v2 = Just (v1, CVar v2)
          vv (AnonFlex _) _ = Nothing
renameM vs e = do
    vs' <- mapM freshVarFrom vs
    let e' = subst (renameSubst vs vs') e
    return (e', vs')

negreduce (CNot e) =
    let (u, e') = splitExist e
        (t:ts)  = splitAnd e'

        negreduce' CTrue  es =
            case es of
                [] -> return CFail
                _  -> return $ notExists u es

        negreduce' CFail es = return CTrue

        negreduce' e@(CExists _ _ _) es = do
                (e2, xs') <- renameM xs e'
                return $ notExists (u ++ xs') (e2:es)
            where (xs, e') = splitExist e

        negreduce' (COr i e1 e2) es = return $ CAnd i (b e1) (b e2)
            where b e = notExists u (e:es)

        {- equality -}
        negreduce' (CEq e1 e2) es = ifte (unify e1 e2) nonvalid valid
            where diseq = notExists u [(CEq e1 e2)]
                  valid = return CTrue

                  nonvalid theta
                        | isPrimDisEq diseq &&
                          isUnsatisf u theta      = unsatisf theta es
                        | isPrimDisEq diseq &&
                          null es                 = fail "prim disequality"
                        | isPrimDisEq diseq       = chan u (CEq e1 e2) es
                        | otherwise               = unfold theta es

                  unfold theta es = return $ notExists u (x:es)
                        where x = foldAnd $ map (eq theta) $ dom theta
                              eq theta x = CEq (CVar x) (subst theta (CVar x))

                  unsatisf [] es = return $ notExists u es
                  unsatisf theta@[(x, e)] es =
                        return $ notExists u $ map (subst theta') es
                    where theta' | x `elem` u = theta
                                 | otherwise     = case e of
                                                      CVar y -> bind y (CVar x)
                                                      _ -> fail "not unsatisf"
                  chan vs eq@(CEq _ _) es =
                        return $ COr (Rho_pi Pi_o) (CNot (exists vs1 eq))
                                 (exists vs1 (CAnd (Rho_pi Pi_o) eq (notExists vs2 es)))
                     where (vs1, vs2) = partition (\v -> v `elem` (fv eq)) vs


        negreduce' e@(CNot ee) es = ifte (negreduce e) reducable rest
            where reducable e' = return $ notExists u (e':es)
                  rest | isVarApp e    = handleHigher u e es
                       | isPrimDisEq e = handleDisequal
                       | otherwise     = error "WTF"
                  handleDisequal | all (`notElem` u) (fv e) = return $ COr (Rho_pi Pi_o) ee (notExists u es)
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
            let args' (CNot e) = args' e'
                    where (_, e') = splitExist e
                args' e = args e
                functor' (CNot e) = functor' e'
                    where (_, e') = splitExist e
                functor' e = functor e

                aux | (functor' e) `isIn` vs = aux1
                    | not (null es)          = aux2
                    | otherwise              = aux3 e

                isIn (CVar v) vs = v `elem` vs

                aux1 = do
                    (_, s) <- resolveFlex e
                    return $ notExists u $ map (subst s) es {- must include the new var R' to "u" -}
                aux2 =
                    let (vs1, vs2) = partition (\v -> v `elem` (fv e)) vs
                    in return $ COr (Rho_pi Pi_o) (CNot (exists vs1 e))
                                (CAnd (Rho_pi Pi_o) (exists vs1 (CAnd (Rho_pi Pi_o) e (notExists vs2 es)))
                                     (notExists vs (e:es)))

                aux3 (CNot e) =
                    let (vs, e') = splitExist e
                    in if null vs
                       then fail "not reducable"
                       else do
                            (e'', vs') <- renameM vs e'
                            return $ exists vs $ notExists u $ [CNot e', notExists vs' [e''] ]
                aux3 _ = fail "not reducable"

            in aux

        isVarApp (CNot e) = isVarApp e'
            where (_, e') = splitExist e
        isVarApp e = isVar $ functor e

    in negreduceSeq t ts []

negreduce _ = fail "not a negative expression"

reduce e =
    case functor e of
        CPred _ _      -> resolveRigid e
        CLambda _ _ _  -> lambdaReduce e
        CAnd _ _ _     -> expandApp e
        COr  _ _ _     -> expandApp e
        _ -> fail "cannot reduce"

expandApp (CApp t (CAnd tAnd e1 e2) e) = return $ CAnd t (CApp t e1 e) (CApp t e2 e)
expandApp (CApp t (COr  tOr e1 e2) e)  = return $ COr  t (CApp t e1 e) (CApp t e2 e)
expandApp (CApp t e1 e)          = do
    e1' <- (expandApp e1)
    expandApp (CApp t e1' e)
expandApp e                   = fail ("expandApp")

returnSuccess e = return (e, success)

derive :: (MonadLogic m, MonadFreeVarProvider m, MonadClauseProvider m) => CExpr -> m (CExpr,Subst)
derive CFail = fail "false"

derive e =
    let deriveSingle (CEq e1 e2) = do
            theta <- unify e1 e2
            return (CTrue, theta)

        deriveSingle (COr _ e1 e2) = mplus (b e1) (b e2)
            where b = returnSuccess

        deriveSingle e@(CExists _ _ _) = do
            (e'', _) <- renameM vs e'
            returnSuccess e''
           where (vs, e') = splitExist e

        deriveSingle e@(CNot _) | isVarApp' e = do
                            (e', s) <- resolveFlex e
                            return (CTrue, s)
                               | otherwise =  negreduce e >>= returnSuccess

        deriveSingle e | isVarApp' e = do
                            (e', s) <- resolveFlex e
                            return (CTrue, s)
                       | otherwise   = reduce e >>= returnSuccess

        deriveSeq e es d = ifte (deriveSingle e) succ f
            where succ (CFail, ss) = return (CFail, ss)
                  succ (CTrue, ss)  = return (subst ss (foldAnd (d ++ es)),   ss)
                  succ (e', ss) = return (subst ss (foldAnd (e':(d ++ es))), ss)
                  f | null es   = fail "all disequalities"
                    | isPrimDisEq e = deriveSeq (head es) (tail es) (e:d)
                    | otherwise = fail ""

        isVarApp' (CNot e) = isVarApp'' e'
            where (_, e') = splitExist e
        isVarApp' e = isVarApp'' e

        isVarApp'' (CNot e) = isVar (functor e)
        isVarApp'' e = isVar (functor e)

        es = splitAnd e

    in deriveSeq (head es) (tail es) []


substFunc (CApp t e a) b = (CApp t (substFunc e b) a)
substFunc _ b = b

resolveRigid g =
    let lam e ty@(Rho_pi (Pi_fun as b)) = do
            l <- lam e (Rho_pi b)
            vs <- mapM freshVarOfType as
            return (CLambda ty vs l)
        lam e ty = return e
        body ty [] = lam CFail (typeOf (functor g))
        body ty bs = return $ foldl1 (COr ty) bs
        clauses (CPred ty sym) = clausesOf sym
    in do
       es <- clauses (functor g)
       e  <- body (typeOf (functor g)) es -- FIXME
       return $ substFunc g e

lambdaReduce :: (Monad m, MonadFreeVarProvider m) => CExpr -> m (CExpr)
lambdaReduce = betaReduce

alphaConvert (CLambda ty xs e) = do
    xs' <- mapM freshVarFrom xs
    e' <- alphaConvert e
    let bindings = zip xs xs'
    let theta = map (\(v, y) -> (v, CVar y)) $ filter (\(x,y) -> isNamedCVar (CVar x)) bindings
    return $ CLambda ty xs' (subst theta e')
alphaConvert (CExists t x e) = do
    x' <- freshVarFrom x
    e' <- alphaConvert e
    let theta = case x of
                   Flex _ v -> bind x (CVar x')
                   AnonFlex _ -> []
    return $ CExists t x' (subst theta e')
alphaConvert e = return e

betaReduce :: (Monad m, MonadFreeVarProvider m) => CExpr -> m (CExpr)
betaReduce (CApp t e as) = do
    e' <- betaReduce e
    case e' of
        CLambda ty xs e'' ->
           -- FIXME : also check that fv(e'') will remain free after substitution
           let theta = filter (\(x,y) -> isNamedCVar (CVar x)) $ zip xs as
           in return $ subst theta e''
        _ ->
           return $ (CApp t e' as)
betaReduce e = alphaConvert e


resolveFlex (CNot e) = resolveFlexNot' vs e'
    where (vs, e') = splitExist e
          resolveFlexNot' vs (CNot e) = resolveFlexPos vs e
          resolveFlexNot' vs e       = resolveFlexNeg vs e
resolveFlex e = resolveFlexPos [] e

resolveFlexNeg vs g =
    let neg vs (CLambda t x e) = CLambda t x (neg vs e)
        neg vs e = CNot (exists vs' e')
            where vs'  = map snd vs
                  vs'' = map (\(x,y) -> (x, CVar y)) vs
                  e'   = subst vs'' e
    in case functor g of
           CVar x ->
               singleInstance (typeOf x) >>- \fi -> do
               fi' <- instantiate fi g
               r <- freshVarFrom x
               vs' <- mapM (\x -> freshVarFrom x >>= \v -> return (x, v)) vs
               let b  = neg vs' fi'
               let s  = bind x (CAnd (typeOf r) b (CVar r))
               let g' = subst s (substFunc g b)
               return (g', s)

resolveFlexPos vs g =
    let pos vs (CLambda t x e) = CLambda t x (pos vs e)
        pos vs e = (exists  vs' e')
            where vs'  = map snd vs
                  vs'' = map (\(x,y) -> (x, CVar y)) vs
                  e'   = subst vs'' e
    in case functor g of
           CVar x ->
               singleInstance (typeOf x) >>- \fi -> do
               fi' <- instantiate fi g
               r <- freshVarFrom x
               vs' <- mapM (\x -> freshVarFrom x >>= \v -> return (x, v)) vs
               let b  = pos vs' fi'
               let s  = bind x (COr (typeOf r) b (CVar r))
               let g' = subst s (substFunc g b)
               return (g', s)
           _ -> fail "resolveF: cannot resolve a non flexible"


instantiate f g =
    let l (CLambda _ x e) = x:(l e)
        l _ = []
        s (CEq (CVar v) e) =
            let sl = concatMap (\(xs',ys') -> zip xs' ys') $ zip (l f) (args g)
            in  case lookup v sl of
                  Just e' -> (CEq (CVar v) e')
                  Nothing -> (CEq (CVar v) e)
        s (CAnd ty e1 e2) = CAnd ty (s e1) (s e2)
        s (CLambda ty x e) = CLambda ty x (s e)
        s e = e
    in return (s f)


basicInstance ty@(Rho_pi (Pi_fun _ _)) =
    msum (map return [1..])          >>- \n ->
    replicateM n (singleInstance ty) >>- \le ->
    return $ foldl1 (COr ty) le
basicInstance x = singleInstance x

singleInstance ty@(Rho_pi (Pi_fun ty_args ty_res)) =
    let argExpr (Rho_pi (Pi_fun _ _)) x
          = msum (map return [1..])        >>- \n ->
            replicateM n (appInst (CVar x)) >>- \le ->
            return $ foldl1 (CAnd tyBool) le
        argExpr ty x | ty == tyAll  = do { y <- singleInstance ty
                                         ; return $ CEq (CVar x) y
                                         }
                     | ty == tyBool = do { y <- singleInstance ty
                                         ; return $ if y == CTrue then (CVar x) else y
                                         }
                     | otherwise    = fail ""
        comb e' (CLambda t x e) = (CLambda t x (comb e' e))
        comb e' e            = CAnd (typeOf e') e' e
        appInst e = do
             case typeOf e of
                 Rho_pi (Pi_fun tyargs tyres) -> do
                      argVars <- mapM freshVarOfType tyargs
                      let args = map CVar argVars
                      appInst (CApp (Rho_pi tyres) e args)
                 _ -> return e
    in do
       xs   <- mapM freshVarOfType ty_args
       xes  <- mapM (\(ty,x) -> argExpr ty x) $ zip ty_args xs
       let xe = foldl1 (CAnd tyBool) xes
       if ((Rho_pi ty_res) == tyBool)
        then return (CLambda ty xs xe)
        else singleInstance (Rho_pi ty_res) >>- \res ->
             return (CLambda ty xs (comb xe res))

singleInstance ty
    | ty == tyBool = return CFail `mplus` return CTrue
    | ty == tyAll  = freshVarOfType ty >>= \x -> return (CVar x)
    | otherwise    = fail "cannot instantiate from type"
