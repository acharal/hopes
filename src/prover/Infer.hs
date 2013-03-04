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

import Logic (runLogicT, observe, LogicT)
import Types (hasType, HasType)
import Subst (restrict, combine, success)
import ComputedAnswer
import Lang

import CoreLang (Expr(..), Program, fv, splitExist)
import qualified CoreLang


-- import Control.Monad (msum, mplus, replicateM)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (StateT, get, modify, evalStateT)
import Control.Monad.Trans (lift)

import Infer.Class

import Derive (derive)

import Debug.Trace (trace)
import Pretty


(>>-) = (>>=)


type Infer m a = ReaderT (Program a) (StateT Int (LogicT m))

runInfer p m = runLogicT Nothing $ evalStateT (runReaderT m p) 0

infer :: Monad m => Program a -> Infer m a b -> m (Maybe (b, Infer m a b))
infer p m =  observe $ evalStateT (runReaderT (msplit m) p) 0




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

{-

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

-}
-- unification


instance (Symbol a, HasType a, Monad m) => MonadFreeVarProvider a (StateT Int m) where
    freshVarOfType ty = do
        a' <- get
        modify (+1)
        return $ hasType ty $ liftSym ("V" ++ show a')

instance (Symbol a, HasType a, Monad m, MonadFreeVarProvider a m) => MonadFreeVarProvider a (ReaderT s m) where
    freshVarOfType = lift . freshVarOfType

instance (Symbol a, Eq a, Monad m) => MonadClauseProvider a (ReaderT (Program a) m) where
    clausesOf r = asks (CoreLang.clausesOf r)

instance (Symbol a, Eq a, Monad m, MonadClauseProvider a m) => MonadClauseProvider a (StateT s m) where
    clausesOf = lift . clausesOf

