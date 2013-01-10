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

import Logic (runLogic, observe, LogicT, msplit, call, cut, ifte, once)
import Types (MonoTypeV(..), tyBool, tyAll, typeOf, hasType)
import Subst (subst, bind, restrict, combine, success)
import Lang (liftSym)

import CoreLang (Expr(..), Program, fv, functor)
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
    return (restrict (fv g) ans)

-- do a refutation
-- refute :: Goal a -> Infer a (Subst a)
refute =  refute' -- ' 30

refute'' n g
    | g == CTrue || n == 0 = return success
    | otherwise = trace (show (ppr g) ++ "\n") $ derive g   >>= \(g',  s)  ->
                   refute'' (n-1) g' >>= \ans ->
                   return (s `combine` ans)

refute' g
    | g == CTrue = return success
    | otherwise  = trace (show (ppr g) ++ "\n") $ derive g   >>= \(g',  s)  ->
                   refute' g' >>= \ans ->
                   return (s `combine` ans)

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


reduce (Not (Not e)) = return (e, success)
reduce (Exists v e)  = do
    v' <- freshVarOfType (typeOf v)
    return (subst (bind v (Var v')) e, success)
reduce e = 
    case functor e of 
        Rigid _    -> resolveRigid e
        Lambda _ _ -> lambdaReduce e        
        And _ _    -> return (expandApp e, success)
        Or  _ _    -> return (expandApp e, success)
        _ -> error $ "cannot reduce" ++ show (ppr e)

expandApp (App (And e1 e2) e) = And (App e1 e) (App e2 e)
expandApp (App (Or  e1 e2) e) = Or  (App e1 e) (App e2 e)
expandApp (App e1 e)          = expandApp (App (expandApp e1) e)
expandApp e                   = error "expandApp"


-- single step derivation
-- derive :: Goal a -> Infer a (Goal a, Subst a)
derive (CFalse) = fail ""
derive (CTrue)  = return (CTrue, success)
derive (Cut)    = return (CTrue, success) -- cut >> return (CTrue, success)

derive (Eq  e1 e2)   = do
    mgu <- unify e1 e2
    return (CTrue, mgu)

derive (And CTrue e) = return (e, success)
derive (And e CTrue) = return (e, success)
derive (And e1 e2)   = do
    (e1',theta) <- derive e1
    return ((And e1' (subst theta e2)), theta)

derive (Or  e1 e2)   = mplus (return (e1, success)) (return (e2, success))

derive e@(Exists _ _) = reduce e

derive (Not CTrue)   = fail ""
derive (Not CFalse)  = return (CTrue, success)

derive e@(Not (Not _)) = reduce e

derive (Not e) = do
    (e',s) <- neg_derive e
    return (Not e', s)

derive e = 
    case functor e of 
        Var _ -> resolveFlex e
        _     -> reduce e


neg_derive (CFalse) = return (CFalse, success)
neg_derive (CTrue)  = fail ""

neg_derive (Eq e1 e2) =  ifte (unify e1 e2) (\s -> fail "") (return (CFalse, success))

neg_derive (Or CFalse e) = return (e, success)
neg_derive (Or e CFalse) = return (e, success)
{-
neg_derive (Or e1 e2) = do
    (e1', s1) <- neg_derive e1
    return (Or e1' (subst s1 e2), s1)
-}

neg_derive (Or e1 e2)  = ifte (neg_derive e1 >>= \(e,s) -> return (Or e (subst s e2), success)) return
                              (neg_derive e2 >>= \(e,s) -> return (Or (subst s e1) e, success))

neg_derive (And CFalse e) = return (CFalse, success)
neg_derive (And e CFalse) = return (CFalse, success)
neg_derive (And e1 e2) = -- derive (Or e1 e2)
--    mplus (neg_derive e2) (neg_derive e1)
    ifte (neg_derive e1 >>= \(e,s) -> return (And e (subst s e2), s)) return
         (neg_derive e2 >>= \(e,s) -> return (And (subst s e1) e, s))

neg_derive (Not CTrue)  = return (CFalse, success)
neg_derive (Not CFalse) = fail ""

neg_derive (Not (Not e)) = return (e, success)
neg_derive (Not e) = do
    (e', s) <- derive e
    return (Not e', s)

neg_derive e =
    case functor e of 
        Var _ -> resolveFlexNot e
        _     -> reduce e

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
