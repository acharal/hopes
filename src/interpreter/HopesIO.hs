
module HopesIO where

import Types
import CoreLang
import Language.Hopl
import Language.Hopl.Syntax (HpSymbol)

import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import Data.Monoid

data HopeEnv =
    HEnv {  
        currentEnv :: TyEnv HpSymbol,
        kb         :: KnowledgeBase (Typed HpSymbol),
        p          :: Program (Typed HpSymbol),
        debugFlag  :: Bool
    }

type HopesIO = StateT HopeEnv IO

-- we like for HopesIO to be MonadIO and MonadState HopeEnv

runDriverM m = evalStateT m tabulaRasa
    where tabulaRasa = HEnv { currentEnv = mempty 
                            , kb = mempty
                            , p  = mempty 
                            , debugFlag = False
                            }
