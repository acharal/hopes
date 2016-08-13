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
module Infer (InferT,runInfer, infer, prove, module ComputedAnswer) where


import Logic (runLogicT, observe)
import Logic (LogicT)
import Types (hasType, HasType)
import Subst (Subst, restrict, combine, success)
import ComputedAnswer
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (liftM, ap)

import Core (CExpr(..), fv, splitExist, KnowledgeBase, Flex(..))
import qualified Core (clausesOf)

-- import Control.Monad (msum, mplus, replicateM)
import Control.Monad.Reader
import Control.Monad.State

import Infer.Class
import Trace.Class

import Derive (derive)

(>>-) :: Monad m => m a -> (a -> m b) -> m b
(>>-) = (>>=)

newtype InferT m a = InferT { unInferT :: ReaderT (KnowledgeBase) (StateT Int (LogicT m)) a }

runInfer p m = runLogicT Nothing $ evalStateT (runReaderT (unInferT m) p) 0

infer :: Monad m => KnowledgeBase -> InferT m a -> m (Maybe (a, InferT m a))
infer p m =  observe $ evalStateT (runReaderT (unInferT (msplit m)) p) 0



-- ifte m th el = call $ (m >> cut >> th) `mplus` el
-- ifte' m th el = call (m >>= \s -> cut >> th s `mplus` el) --call $ (m >>= \s -> cut >> th s) `mplus` el




-- ifte m th el = call $ (m >> cut >> th) `mplus` el
-- ifte' m th el = call (m >>= \s -> cut >> th s `mplus` el) --call $ (m >>= \s -> cut >> th s) `mplus` el

-- try prove a formula by refutation
prove  :: (MonadTrace (CExpr, Subst) m,
           Monad m) => CExpr -> InferT m (ComputedAnswer)
prove g =  do
    ans <- refute g
    answer (fv g) ans

answer fv (g,ans) = return $ Computed (restrict fv ans) (splitAnd g)
    where splitAnd (CAnd _ e1 e2) = splitAnd e1 ++ splitAnd e2
          splitAnd CTrue = []
          splitAnd e = [e]

-- do a refutation
refute :: (Monad m,
           MonadTrace (CExpr, Subst) m) => CExpr -> InferT m (CExpr, Subst)
refute =  refute''' --' 50

refute''' :: (Monad m,
              MonadTrace (CExpr, Subst) m) => CExpr -> InferT m (CExpr, Subst)
refute''' CTrue = return (CTrue, success)
refute''' g = ifte (derive g) cont failed
    where cont (g,s) = do
            lift $ trace (g, s)
            (g', s') <- refute''' g
            return (g', s `combine` s')
          failed = if isSuccessful g
                   then return (g, success)
                   else fail "not successful goal"
          isSuccessful CTrue = True
          isSuccessful (CAnd _ e1 e2) = isSuccessful e1 && isSuccessful e2
          isSuccessful (CNot e) =
            let (v, e') = splitExist e
            in case e' of
                  CEq e1 e2 -> True
                  _ -> False
          isSuccessful _ = False


instance Monad m => Functor (InferT m) where
    fmap = liftM

instance Monad m => Applicative (InferT m) where
    pure a = InferT $ return a
    (<*>) = ap

instance Monad m => Monad (InferT m) where
    return = pure
    m >>= f = InferT $ (unInferT m >>= \a -> unInferT (f a))
    fail a = InferT $ fail a

instance MonadTrans (InferT) where
    lift = InferT . lift . lift . lift

instance Monad m => Alternative (InferT m) where
    (<|>) = mplus
    empty = mzero

instance Monad m => MonadPlus (InferT m) where
    mzero = InferT mzero
    mplus m1 m2 = InferT (mplus (unInferT m1) (unInferT m2))

instance Monad m => MonadLogic (InferT m) where
    msplit m = InferT $ do
        r <- msplit (unInferT m)
        case r of
            Nothing -> return Nothing
            Just (a, s) -> return (Just (a, InferT s))

instance MonadIO m => MonadIO (InferT m) where
    liftIO = InferT .  liftIO

instance MonadState s m => MonadState s (InferT m) where
    get = lift $ get
    put = lift . put

instance Monad m => MonadFreeVarProvider (InferT m) where
    freshVarOfType ty = InferT $ do
        a' <- get
        modify (+1)
        return $ Flex ty ("V" ++ show a')
    freshVarFrom v@(Flex ty x) = InferT $ do
        a' <- get
        modify (+1)
        return $ Flex ty ("V" ++ show a')
    freshVarFrom (AnonFlex ty) = InferT (return (AnonFlex ty))

instance Monad m => MonadClauseProvider (InferT m) where
    clausesOf r = InferT $ asks (\b -> Core.clausesOf b r)
