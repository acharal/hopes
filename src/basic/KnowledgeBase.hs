--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module KnowledgeBase where

import Lang
import Hopl
-- import Types
import Data.Monoid
-- import Control.Monad.State
-- import Control.Monad.Reader

data KnowledgeBase a =
    KB {
        -- name :: String
        clauses :: [Clause a]
    }

instance HasSignature (KnowledgeBase a) a where
	sig cs = mconcat $ map sig $ clauses cs

instance Monoid (KnowledgeBase a) where
    mempty  = KB mempty
    mappend (KB x) (KB y) = KB $ mappend x y


{-

assert :: MonadState (KnowledgeBase a) m => Clause a -> m ()
assert c = do
    kb <- gets clauses
    modify (\s -> s { clauses = (c:kb) })

retract :: (Eq a, MonadState (KnowledgeBase a) m) => Clause a -> m ()
retract c = do
    kb <- gets clauses
    modify (\s -> s { clauses = filter (c==) kb })

branch :: (MonadReader m, MonadPlus m) => a ->  m (KnowledgeBase a) Clause

typeof :: (MonadReader m) => a ->  m (KnowledgeBase a) Type

-}