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


module Logic.SFK(
    module Logic.SFK,
    module Logic.Class
)where

import Control.Monad
import Control.Monad.Trans
import Logic.Class


newtype LogicT m a  = SFKT (forall ans. SK (m ans) a -> FK (m ans) -> m ans)
unSFKT (SFKT a) = a

type FK ans = ans
type SK ans a = a -> FK ans -> ans

instance MonadTrans LogicT where
    lift m = SFKT (\sk fk -> m >>= (\a -> sk a fk))

instance Monad m => Monad (LogicT m) where
    return e = SFKT (\sk fk -> sk e fk)
    m >>= f  =
      SFKT (\sk fk ->
           unSFKT m (\a fk' -> unSFKT (f a) sk fk')
             fk)
    fail s   = SFKT (\_ fk -> fk)

instance Monad m => MonadPlus (LogicT m) where
    mzero = SFKT (\_ fk -> fk)
    m1 `mplus` m2 = SFKT (\sk fk -> unSFKT m1 sk (unSFKT m2 sk fk))

instance MonadIO m => MonadIO (LogicT m) where
    liftIO = lift . liftIO

instance Monad m => MonadLogic (LogicT m) where
    msplit m = lift $ unSFKT m ssk (return Nothing)
        where ssk a fk = return $ Just (a, (lift fk >>= reflect))


-- observe . lift = id
observe m = unSFKT m (\a fk -> return a) (fail "no answer")

runL n m = observe (bagofN n m)

runLogicT :: (Monad m) => Maybe Int -> LogicT m a -> m [a]
runLogicT  Nothing (SFKT m) = m (\a fk -> fk >>= (return . (a:))) (return [])
runLogicT (Just n) (SFKT m) | n <=0 = return []
runLogicT (Just 1) (SFKT m) = m (\a fk -> return [a]) (return [])
runLogicT (Just n) m = unSFKT (msplit m) runM' (return [])
    where runM' Nothing _ = return []
          runM' (Just (a,m')) _ = runLogicT (Just (n-1)) m' >>= (return . (a:))
