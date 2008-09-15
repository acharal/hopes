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

{-# OPTIONS -fglasgow-exts #-}

module Logic.Class where

import Control.Monad.Reader
import Control.Monad.State

class MonadPlus m => MonadLogic m where
    msplit :: m a -> m (Maybe (a, m a))
    interleave :: m a -> m a -> m a
    interleave sg1 sg2 = do
        r <- msplit sg1
        case r of
            Nothing -> sg2
            Just (a,b) ->
                (return a) `mplus` (interleave sg2 b)
    (>>-) :: m a -> (a -> m b) -> m b
    sg >>- g = do
        r <- msplit sg
        case r of
            Nothing -> mzero
            Just (a,b) -> interleave (g a) (b >>- g)
    ifte  :: m a -> (a -> m b) -> m b -> m b
    ifte t th el = do
        r <- msplit t
        case r of
            Nothing -> el
            Just (a,b) -> (th a) `mplus` (b >>= th)
    once  :: m a -> m a
    once m = do
        r <- msplit m
        case r of
            Nothing -> mzero
            Just (a,_) -> return a

-- | this is the opposite of msplit
-- | The law is : msplit tm >>= reflect = tm
reflect r = case r of
                Nothing -> mzero
                Just (a,tmr) -> return a `mplus` tmr


instance MonadLogic m => MonadLogic (StateT s m) where
    msplit (StateT m) =
        StateT $ \s -> 
            msplit (m s) >>= \r ->
                case r of 
                    Nothing -> return (Nothing, s)
                    Just ((a, s'), m') ->
                        return (Just (a, StateT $ \s -> m' ), s')

instance MonadLogic m => MonadLogic (ReaderT r m) where
    msplit (ReaderT m) = 
        ReaderT $ \env -> 
            msplit (m env) >>= \r -> 
                case r of
                    Nothing -> return Nothing
                    Just (a,m') -> return (Just (a, ReaderT $ \env' -> m'))

bagofN (Just n) _ | n <= 0  = return []
bagofN n m = msplit m >>= bagofN'
    where bagofN' Nothing = return []
          bagofN' (Just (a,m')) = bagofN (fmap (-1 +) n) m' >>= (return . (a:))