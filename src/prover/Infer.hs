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

{-# LANGUAGE
    FlexibleContexts
   ,FlexibleInstances
   ,MultiParamTypeClasses
   ,NoMonomorphismRestriction
   ,UndecidableInstances
#-}

-- | Proof procedure of Hopl
module Infer (runInfer, infer, prove) where


import Logic (runLogicT, observe)
import Logic (LogicT)
import Types (hasType, HasType)
import Subst (restrict, combine, success)
import ComputedAnswer
import Lang
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (liftM, ap)

import CoreLang (Expr(..), Program, fv, splitExist)

import qualified CoreLang


-- import Control.Monad (msum, mplus, replicateM)
import Control.Monad.Reader
import Control.Monad.State

import Infer.Class
import Trace.Class

import Derive (derive)

(>>-) = (>>=)

newtype InferT a m b = InferT { unInferT :: ReaderT (Program a) (StateT Int (LogicT m)) b }

runInfer p m = runLogicT Nothing $ evalStateT (runReaderT (unInferT m) p) 0

infer :: Monad m => Program a -> InferT a m b -> m (Maybe (b, InferT a m b))
infer p m =  observe $ evalStateT (runReaderT (unInferT (msplit m)) p) 0



-- ifte m th el = call $ (m >> cut >> th) `mplus` el
-- ifte' m th el = call (m >>= \s -> cut >> th s `mplus` el) --call $ (m >>= \s -> cut >> th s) `mplus` el




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

refute''' CTrue = return (CTrue, success)
refute''' g = ifte (derive g) cont failed
    where cont (g,s) = do
            trace (g, s)
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


instance Monad m => Functor (InferT a m) where
    fmap = liftM

instance Monad m => Applicative (InferT a m) where
    pure a = InferT $ return a
    (<*>) = ap

instance Monad m => Monad (InferT a m) where
    return = pure
    m >>= f = InferT $ (unInferT m >>= \a -> unInferT (f a))
    fail a = InferT $ fail a

instance MonadTrans (InferT a) where
    lift = InferT . lift . lift . lift

instance Monad m => Alternative (InferT a m) where
    (<|>) = mplus
    empty = mzero

instance Monad m => MonadPlus (InferT a m) where
    mzero = InferT mzero
    mplus m1 m2 = InferT (mplus (unInferT m1) (unInferT m2))

instance Monad m => MonadLogic (InferT a m) where
    msplit m = InferT $ do
        r <- msplit (unInferT m)
        case r of
            Nothing -> return Nothing
            Just (a, s) -> return (Just (a, InferT s))

instance MonadIO m => MonadIO (InferT a m) where
    liftIO = InferT .  liftIO

instance MonadState s m => MonadState s (InferT a m) where
    get = lift $ get
    put = lift . put

instance (Symbol a, HasType a, Monad m) => MonadFreeVarProvider a (InferT a m) where
    freshVarOfType ty = InferT $ do
        a' <- get
        modify (+1)
        return $ hasType ty $ liftSym ("V" ++ show a')

instance (Symbol a, Eq a, Monad m) => MonadClauseProvider a (InferT a m) where
    clausesOf r = InferT $ asks (CoreLang.clausesOf r)
