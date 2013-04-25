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

import Logic (runLogicT, observe, LogicT)
import Types (hasType, HasType)
import Subst (restrict, combine, success)
import Lang


import CoreLang (Expr(..), Program, fv)
import qualified CoreLang

-- import Control.Monad (msum, mplus, replicateM)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (StateT, get, modify, evalStateT)
import Control.Monad.Trans (lift)

import Infer.Class

import Derive (derive)

type Infer m a = ReaderT (Program a) (StateT Int (LogicT m))

runInfer p m = runLogicT Nothing $ evalStateT (runReaderT m p) 0

infer :: Monad m => Program a -> Infer m a b -> m (Maybe (b, Infer m a b))
infer p m =  observe $ evalStateT (runReaderT (msplit m) p) 0

-- try prove a formula by refutation
-- prove  :: Goal a -> Infer a (Subst a)
prove g =  do
    ans <- refute g
    return (restrict (fv g) ans)

-- do a refutation
-- refute :: Goal a -> Infer a (Subst a)
refute g
    | g == CTrue = return success
    | otherwise  = derive g  >>- \(g',  s)  ->
                   refute g' >>- \ans ->
                   return (s `combine` ans)


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
