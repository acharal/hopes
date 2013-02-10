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
import Subst (subst, bind, restrict, combine, success, dom)
import Lang (liftSym)

import CoreLang (Expr(..), Program, fv, functor, splitExist, exists)
import ComputedAnswer
import qualified CoreLang (clausesOf)

import Control.Monad (msum, mplus, when, replicateM)
import Control.Monad.Reader (asks, runReaderT, ReaderT)
import Control.Monad.State (get, modify, evalStateT, StateT)
import Control.Monad.Identity (runIdentity, Identity)

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
refute =  refute''' --' 50

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

refute''' CTrue = return (CTrue, success)
refute''' g = ifte (derive' g) cont failed
    where derive' g  = trace (show (ppr g) ++ "\n") $ derive g
          cont (g,s) = do
            (g', s') <- refute''' g
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
{-
reduce (Exists v e)  = do
    v' <- freshVarOfType (typeOf v)
    return (subst (bind v (Var v')) e, success)
-}
reduce e = 
    case functor e of 
        Rigid _    -> resolveRigid e
        Lambda _ _ -> lambdaReduce e
        And _ _    -> return $ expandApp e
        Or  _ _    -> return $ expandApp e
        _ -> error $ "cannot reduce" ++ show (ppr e)

expandApp (App (And e1 e2) e) = And (App e1 e) (App e2 e)
expandApp (App (Or  e1 e2) e) = Or  (App e1 e) (App e2 e)
expandApp (App e1 e)          = expandApp (App (expandApp e1) e)
expandApp e                   = error "expandApp"


returnSuccess e = return (e, success)

-- single step derivation
-- derive :: Goal a -> Infer a (Goal a, Subst a)
derive (CFalse) = fail ""
derive (CTrue)  = returnSuccess CTrue
derive (Cut)    = returnSuccess CTrue -- cut >> return (CTrue, success)

derive (Eq  e1 e2)   = unifyAndReturn e1 e2

derive (And CFalse e) = returnSuccess CFalse
derive (And e CFalse) = returnSuccess CFalse

derive (And CTrue e) = returnSuccess e
derive (And e CTrue) = returnSuccess e
derive (And e1 e2)   = do
    (e1',theta) <- derive e1
    return ((And e1' (subst theta e2)), theta)

derive (Or  e1 e2)   = mplus (returnSuccess e1) (returnSuccess e2)

derive (Not CTrue)   = fail ""
derive (Not CFalse)  = returnSuccess CTrue

derive e@(Not (Not _)) = reduce e >>= returnSuccess

derive (Not e) = do
    (e',s) <- neg_derive e
    return (Not e', s)

derive (Exists v e)  = do
    v' <- freshVarOfType (typeOf v)
    returnSuccess $ subst (bind v (Var v')) e

derive e = 
    case functor e of 
        Var _ -> resolveFlex e
        _     -> reduce e >>= returnSuccess


neg_derive (CFalse) = returnSuccess CFalse
neg_derive (CTrue)  = fail ""

neg_derive (Eq e1 e2) =  ifte (unify e1 e2) (\_ -> fail "") (returnSuccess CFalse)

neg_derive (Or CTrue e) = returnSuccess CTrue
neg_derive (Or e CTrue) = returnSuccess CTrue

neg_derive (Or CFalse e) = returnSuccess e
neg_derive (Or e CFalse) = returnSuccess e

neg_derive (Or e1 e2)  = ifte (neg_derive e1 >>= \(e,s) -> return (Or e (subst s e2), s)) return
                              (neg_derive e2 >>= \(e,s) -> return (Or (subst s e1) e, s))

{-
neg_derive (And CFalse e) = return (CFalse, success)
neg_derive (And e CFalse) = return (CFalse, success)
-}

neg_derive (And e1 e2) =
      mplus (returnSuccess e1) (returnSuccess e2)
--    mplus (neg_derive e1) (neg_derive e2)
--    ifte (neg_derive e1 >>= \(e,s) -> return (And e (subst s e2), s)) return
--         (neg_derive e2 >>= \(e,s) -> return (And (subst s e1) e, s))

neg_derive (Not CTrue)  = returnSuccess CFalse
neg_derive (Not CFalse) = returnSuccess CTrue

neg_derive e@(Not (Not _)) = reduce e >>= returnSuccess
neg_derive (Not e) = do
    (e', s) <- derive e
    return (Not e', s)

neg_derive e@(Exists _ _) = neg_deriveExist e

neg_derive e =
    case functor e of 
        Var _ -> resolveFlexNot e
        _     -> reduce e >>= returnSuccess

neg_deriveExist e@(Exists _ _) = ifte (reduceExist e) returnSuccess (uncurry a (splitExist e))
    where a vs (And e1@(Eq _ _) e2) = returnSuccess $ And (exists vs' e1) (Not (exists vs' (And e1 (Not (exists vs'' e2)))))
            where vs'  = filter (\v -> v `elem` fv e1) vs
                  vs'' = filter (\v -> v `notElem` fv e1) vs
          a vs (Eq e1 e2)  = ifte (unify e1 e2) return1 (returnSuccess CFalse)
            where return1 mgu = 
                        if unsatisf vs mgu 
                        then fail "return1" 
                        else returnSuccess CTrue
                  unsatisf vs mgu = any (\x -> x `notElem` vs) $ dom mgu
          a vs e = returnSuccess $ exists vs e


reduceExist (Exists v (Or e1 e2)) = return $ Or (Exists v e1) (Exists v e2)

reduceExist (Exists v e) | v `notElem` fv e = return e
                         | otherwise        = do 
                            e' <- reduceExist e
                            return (Exists v e')

reduceExist (And CTrue e) = return e
reduceExist (And e CTrue) = return e
reduceExist (And (Or e1 e2) e) = return $ Or (And e1 e) (And e2 e)
-- reduceExist (And e (Or e1 e2)) = return $ Or (And e e1) (And e e2)
reduceExist (And (And e1 e2) e) = return $ And e1 (And e2 e)
-- reduceExist (And e (And e1 e2)) = return $ And (And e e1) e2

reduceExist (And e1 e2) = do
    e1' <- reduceExist e1
    return (And e1' e2)

reduceExist (Not (And e1 e2)) = return $ Or  (Not e1) (Not e2)
reduceExist (Not (Or e1 e2))  = return $ And (Not e1) (Not e2)
reduceExist e@(Not e') | isReducable e = reduce e
                       | otherwise     = reduceExist e'

reduceExist e | isReducable e = reduce e
              | otherwise     = fail "not reducable"

isReducable e =   
    case functor e of
            Rigid _ -> True
            Lambda _ _ -> True
            Or _ _ -> True
            And _ _ -> True
            Not (Not _) -> True
            _ -> False


unifyAndReturn e1 e2 = ifte (unify e1 e2) true false
    where true mgu = return (CTrue, mgu)
          false    = returnSuccess CFalse


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

resolveFlexNot g = 
    case functor g of
        Var x -> 
            negSingleInstance (typeOf x) >>- \fi -> do
            r <- freshVarOfType (typeOf x)
            let s  = bind x (And fi (Var r))
            let g' = subst s (substFunc g fi)
            return (g', s)
            
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

negSingleInstance ty@(TyFun _ _) = 
    let neg (Lambda x e) = Lambda x (neg e)
        neg e = Not e
    in do
        i <- singleInstance ty
        return $ neg i
negSingleInstance ty = singleInstance ty

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
