--  Copyright (C) 2007 2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Hope where

import Syntax
import Symbol
import Pretty
import Error
import Loc
import Types
import Parser
import WellForm
import Core
import Infer
import Hopl

data HopeEnv = 
    HEnv {  
        currentEnv   :: TyEnv HpSymbol,
        consultedSrc :: Prog (Typed HpSymbol)
    }