--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--
--  Adapted from the paper
--  Backtracking, Interleaving, and Terminating Monad Transformers, by
--  Oleg Kiselyov, Chung-chieh Shan, Daniel P. Friedman, Amr Sabry
--  (http://www.cs.rutgers.edu/~ccshan/logicprog/LogicT-icfp2005.pdf)
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


module Logic.SR (
    module Logic.SR,
    module Logic.Class
) where

import Logic.Class
import Control.Monad
import Control.Monad.Reader
import Control.Monad.CC

data Tree r m a =
      HZero
    | HOne a
    | HChoice a (CCT r m (Tree r m a))

compose_trees HZero r = r
compose_trees (HOne a) r = return $ HChoice a r
compose_trees (HChoice a r') r =
    return $ HChoice a $ r' >>= \v -> compose_trees v r


newtype SR r m a = SR (forall ans. ReaderT (Prompt r (Tree r m ans)) (CCT r m) a)


unSR (SR r) = r

instance Monad m => Monad (SR r m) where
    return e = SR $ return e
    (SR m) >>= f = SR $ m >>= \x -> (unSR (f x))


instance Monad m => MonadPlus (SR r m) where
    mzero = SR $ ask >>= \p -> lift (abort p (return HZero))
    m1 `mplus` m2 = SR $ ask >>= \p ->
        lift $ shift p $ \sk -> do
            f1 <- sk (runReaderT (unSR m1) p)
            let f2 = sk (runReaderT (unSR m2) p)
            compose_trees f1 f2

reify :: Monad m => SR r m a -> CCT r m (Tree r m a)
reify m = reset $ \p -> runReaderT (unSR m) p >>= \a -> return (HOne a)


instance MonadTrans (SR r) where
    lift m = SR $ lift $ lift m

instance Monad m => MonadLogic (SR r m) where
    msplit m = SR $ lift (reify m >>= return . reflect_sr)
        where reflect_sr HZero    = Nothing
              reflect_sr (HOne a) = Just (a, mzero)
              reflect_sr (HChoice a r) =
                    Just (a, (SR (lift r)) >>= (return . reflect_sr) >>= reflect)


observe :: Monad m => (forall ans. SR ans m a) -> m a
observe m = runCCT (reify m >>= pick)
    where pick HZero = fail "no answer"
          pick (HOne a) = return a
          pick (HChoice a _) = return a

runLogicT :: Monad m => Maybe Int -> (forall ans. SR ans m a) -> m [a]
runLogicT n m = observe (bagofN n m)

hasBranch m = SR $ lift (reify m >>= check)
    where check (HZero ) = return False
          check (HOne _) = return False
          check (HChoice _ _) = return True
