--  Copyright (C) 2006-2013 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

{-# LANGUAGE MultiParamTypeClasses #-}

module Infer.Class (
    module Infer.Class,
    module Logic.Class
) where


import Logic.Class

import Lang
import Types
import CoreLang


class (Symbol a, HasType a) => MonadFreeVarProvider a m where
    freshVarOfType :: Type -> m a


class Monad m => MonadClauseProvider a m where
    clausesOf :: a -> m [Expr a]
