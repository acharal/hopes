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
module Infer (runInfer, infer, prove) where

import Logic (runLogic, observe, LogicT, msplit, ifte, once)
import Logic (call, cut)
import Types (MonoTypeV(..), tyBool, tyAll, typeOf, hasType)
import Subst (subst, bind, restrict, combine, success, dom, isRenamingSubst, vars)
import Lang (liftSym)

import CoreLang (Expr(..), Program, fv, functor, splitExist, exists, isVar, args)
import ComputedAnswer
import qualified CoreLang (clausesOf)

import Control.Monad (msum, mplus, when, replicateM)
import Control.Monad.Reader (asks, runReaderT, ReaderT)
import Control.Monad.State (get, modify, evalStateT, StateT)
import Control.Monad.Identity (runIdentity, Identity)
import Data.List (partition)

import Debug.Trace (trace)
import Pretty


(>>-) = (>>=)

type Infer a = ReaderT (Program a) (StateT Int (LogicT Identity))

runInfer p m = runIdentity $ runLogic Nothing $ evalStateT (runReaderT m p) 0

infer :: Program a -> Infer a b -> Maybe (b, Infer a b)
infer p m =  runIdentity $ observe $ evalStateT (runReaderT (msplit m) p) 0




-- ifte m th el = call $ (m >> cut >> th) `mplus` el
-- ifte' m th el = call (m >>= \s -> cut >> th s `mplus` el) --call $ (m >>= \s -> cut >> th s) `mplus` el

-- try prove a formula by refutation
-- prove  :: Goal a -> Infer a (Subst a)
prove g =  do
    ans <- refute g
    answer (fv g) ans


answer fv (g,ans) = return $ Computed (restrict fv ans) (splitAnd g)
    where splitAnd (And e1 e2) = splitAnd e1 ++ splitAnd e2
          splitAnd CTrue = []
          splitAnd e = [e]

-- do a refutation
-- refute :: Goal a -> Infer a (Subst a)
refute =  refute''' (-1) --(50)

refute'' n g
    | g == CTrue || n == 0 = return success
    | otherwise =  trace (show (ppr g) ++ "\n") $ 
                   derive g   >>= \(g',  s)  ->
                   refute'' (n-1) g' >>= \ans ->
                   return (s `combine` ans)

refute' g
    | g == CTrue = return success
    | otherwise  = trace (show (ppr g) ++ "\n") $ 
                   derive g   >>= \(g',  s)  ->
                   refute' g' >>= \ans ->
                   return (s `combine` ans)

refute''' n CTrue = return (CTrue, success)
refute''' 0 g = fail ""
refute''' n g = ifte (derive' g) cont failed
    where derive' g  = trace (show (ppr g) ++ "\n") $ derive g
          cont (g,s) = do
            (g', s') <- refute''' (n-1) g
            return (g', s `combine` s')
          failed = if isSuccessful g 
                   then return (g, success)
                   else fail "not successful goal"
          isSuccessful CTrue = True
          isSuccessful (And e1 e2) = isSuccessful e1 && isSuccessful e2
          isSuccessful (Not e) = 
            let (v, e') = splitExist e
            in case e' of
                  Eq e1 e2 -> True
                  _ -> False
          isSuccessful _ = False

refuteRec (CTrue)     = return success
refuteRec (Not e)     = neg_refuteRec e
refuteRec e@(And _ _) = do
    (e1,e2) <- select e
    s1 <- refuteRec e1
    s2 <- refuteRec (subst s1 e2)
    return (s1 `combine` s2)
refuteRec (Or e1 e2)  = refuteRec e1 `mplus` refuteRec e2
refuteRec e =
    case functor e of 
        Rigid _ -> call (refuteRec' e)
        _ -> refuteRec' e
    where refuteRec' e = do
             (e', s') <- derive e
             s'' <- refuteRec e'
             return (s' `combine` s'')

neg_refuteRec (CFalse) = return success
neg_refuteRec (Not e) = refuteRec e
{-
neg_refuteRec e@(Or _ _) =  do
    (e1, e2) <- neg_select e
    s1 <- neg_refuteRec e1
    s2 <- neg_refuteRec (subst s1 e2)
    return (s1 `combine` s2)
-}
neg_refuteRec e@(Or e1 e2)  = ifte (neg_refuteRec e1) (return) (neg_refuteRec e2)
neg_refuteRec e@(And e1 e2) = once (neg_refuteRec e1 `mplus` neg_refuteRec e2)
    -- ifte' (neg_refuteRec e1) (return) (neg_refuteRec e2)
    -- neg_refuteRec e1 `mplus` neg_refuteRec e2
neg_refuteRec e = 
    case functor e of
        Rigid _ -> call (neg_refuteRec' e)
        Var _ -> refuteRec (Not e)
        _ -> neg_refuteRec' e
    where neg_refuteRec' e = do
              (e', s') <- neg_derive e
              s'' <- neg_refuteRec e'
              return (s' `combine` s'')

-- selection rule - always select leftmost
select (And e1@(And _ _) e2) = do 
    (e1', e2') <- select e1
    return (e1', And e2' e2)
select (And e1 e2) = return (e1, e2)
select _ = error "cannot select subexpression"

neg_select (Or e1@(Or _ _) e2) = do 
    (e1', e2') <- neg_select e1
    return (e1', Or e2' e2)
neg_select (Or e1 e2) = return (e1, e2)
neg_select _ = error "cannot select subexpression"


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
expandApp e                   = fail ("expandApp" ++ show (ppr e))


returnSuccess e = return (e, success)

unifyAndReturn e1 e2 = ifte (unify e1 e2) true false
    where true mgu = return (CTrue, mgu)
          false    = returnSuccess CFalse

-- single step derivation
-- derive :: Goal a -> Infer a (Goal a, Subst a)
{-
derive (CFalse) = fail ""
derive (CTrue)  = returnSuccess CTrue
-}
derive (Cut)    = returnSuccess CTrue -- cut >> return (CTrue, success)

derive (Eq  e1 e2)   = unifyAndReturn e1 e2

{-
derive (And CTrue e) = returnSuccess e
derive (And e CTrue) = returnSuccess e
-}
derive e@(And CTrue _)  = reduce e >>= returnSuccess
derive e@(And _ CTrue)  = reduce e >>= returnSuccess
derive e@(And CFalse _) = reduce e >>= returnSuccess
derive e@(And _ CFalse) = reduce e >>= returnSuccess
derive e@(And _ _) = ifte (derive' e) return (reduce e >>= returnSuccess)
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
derive ne@(Not (Not _)) = reduce ne >>= returnSuccess
derive ne@(Not e) = ifte neg return (reduce ne >>= returnSuccess)
    where neg = do
            (e', s) <- neg_derive e
            return (Not e', s)

derive (Exists v e)  = do
    v' <- freshVarOfType (typeOf v)
    returnSuccess $ subst (bind v (Var v')) e

derive e = 
    case functor e of 
        Var _ -> resolveFlex [] e
        _     -> reduce e >>= returnSuccess


neg_derive e = 
    let (vs, e') = splitExist e
    in  neg_derive' vs e'

neg_derive' vs (Eq e1 e2) = ifte (unify e1 e2) nonvalid valid
    where isSatisfiable theta = any (\x -> x `notElem` vs) $ dom theta
          valid  = returnSuccess CFalse
          nonvalid mgu = 
              if isSatisfiable mgu
              then satisfiable mgu
              else returnSuccess CTrue
          satisfiable mgu = 
              if isVar e1
              then fail "satisfiable"
              else msum $ map (equality mgu) $ filter (\x -> x `notElem` vs) $ dom mgu
          equality mgu x = returnSuccess $ exists vs $ Eq (Var x) (subst mgu (Var x))

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
            (e', s) <- derive e
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
    in do
       es <- clausesOf (functor g)
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
--alphaConvert (Forall x e) = do
--    x' <- freshVarOfType (typeOf x)
--    e' <- alphaConvert e
--    return $ Forall x' (subst (bind x (Var x')) e')
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
    where occursIn a e = a `elem` (fv e)

-- utils

clausesOf (Rigid r) = asks (CoreLang.clausesOf r)
clausesOf e = fail "expression must be rigid (parameter)" 

clauseOf e = do
    cl <- clausesOf e
    msum (map return cl)

-- freshVarOfType :: (MonadState Int m, Symbol a, HasType a) => Type -> m a
freshVarOfType ty = do
    a' <- get
    modify (+1)
    return $ hasType ty $ liftSym ("V" ++ show a')
