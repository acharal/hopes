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