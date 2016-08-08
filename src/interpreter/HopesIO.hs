
module HopesIO where

-- import Types
--import CoreLang
--import Language.Hopl
--import Language.Hopl.Syntax (HpSymbol)

import Control.Monad.State.Strict (StateT, evalStateT)

import Data.Monoid

data (Monoid e, Monoid a) => HopeEnv e a = 
    HEnv {  
        currentEnv :: e,
        p          :: a,
--        kb         :: KnowledgeBase (Typed HpSymbol),
        debugFlag  :: Bool
    }

-- type HopesIO = StateT HopeEnv IO

-- we like for HopesIO to be MonadIO and MonadState HopeEnv

runDriverM m = evalStateT m tabulaRasa
    where tabulaRasa = HEnv { currentEnv = mempty
                            , p  = mempty
                            , debugFlag = False
                            }
