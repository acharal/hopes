{-
 - Utility functions for Type checking
 -}

module TcUtils where

import Basic
import Types 
import Parser
import Syntax
import Error
import Data.List
import Data.Maybe (fromJust)
import Data.Graph
import Text.PrettyPrint

import Control.Monad.Reader
import Control.Monad.State


-- | Monomorphic type signature (variables)
type RhoSig a  = (a, RhoType)
-- | Polymorphic type signature (predicates)
type PolySig a = (a, PolyType)

instance HasType (RhoSig a) where
    typeOf (_, t) = t
    hasType ty (a, _) = (a, ty)

-- | type environment is a set of type signatures for
-- variables and polymorphic predicates
data TyEnv a b = 
    TyEnv { rhoSigs  :: [ RhoSig  a ]
          , polySigs :: [ PolySig b ]
          }

lookupRho :: Eq a => a -> TyEnv a b -> Maybe RhoType
lookupRho a = (lookup a).rhoSigs

lookupPoly :: Eq b => b -> TyEnv a b -> Maybe PolyType
lookupPoly a = (lookup a).polySigs



-- Concrete type environment
type PredSig = (Symbol, Int)
type TcEnv = TyEnv Symbol PredSig

-- A constraint is a pair of types with an associated expression. 
-- Convention: first type is the type syntactically assossiated with the expression
type Constraint a = (RhoType, RhoType, SExpr a)

-- Type Checker state
data TcState  = 
    TcState { uniq   :: Int             -- next fresh variable 
            , cnts   :: [Constraint (Typed PosSpan)]  -- generated constraints TODO enrich this with locs.
            , exists :: [RhoSig Symbol] -- existentially quantified vars
            , msgs   :: Messages        -- error messages
            }

-- TypeCheck monad. 
-- Supports state, errors
type Tc m = ReaderT TcEnv (StateT TcState (ErrorT Messages m)) 


{-
 - Auxiliary functions for the Tc monad
 -}

-- Restrict an expression in the Head: no lambdas or predicate
-- constants allowed
-- TODO: error discipline


restrictHead :: Monad m => SHead a -> Tc m ()
restrictHead h =  h |> allHeadExprs  
                    |> mapM_ restrict
    where restrict (SExpr_predCon _ _ _ _) = throwError restrictError
          restrict (SExpr_lam _ _ _)       = throwError restrictError
          restrict _                       = return ()
          restrictError =
              mkMsgs $ mkErr TypeError Fatal $ text "predicate or lambda in head"

-- Add a constraint to the state
addConstraint rho1 rho2 expr = 
    modify (\st -> st { cnts = ( rho1, rho2, expr ) : cnts st})

-- Add an existentially quantified var. to the state 
addExist var tp = 
    modify (\st -> st { exists = (var, tp) : exists st})

-- Fresh variable generation 
newAlpha :: Monad m => Tc m Alpha
newAlpha = do
    st <- get
    let n = uniq st
    put st{uniq = n+1}
    return $ Alpha ('a' : show n)

newPhi :: Monad m => Tc m Phi
newPhi = do
    st <- get
    let n = uniq st
    put st{uniq = n+1}
    return $ Phi ('t' : show n)

newAlphas n = sequence $ replicate n $ newAlpha
newPhis   n = sequence $ take n $ repeat $ newPhi

-- Most general type of arity n as a pi-type
typeWithArity 0 = return Pi_o
typeWithArity n = do
    paramTypes <- newAlphas n
    resType    <- newPhi
    return $ Pi_fun (map (\al -> Rho_var al) paramTypes) (Pi_var resType)

-- Freshen a polymoprhic type TODO: MOCKUP!
freshen :: Monad m => PolyType -> Tc m PiType
freshen _ = return $ error "not implemented yet!"

-- Work with new variables in the environment
withEnvVars :: Monad m => [(Symbol,RhoType)] -> Tc m a -> Tc m a
withEnvVars bindings = 
    local (\env -> env{ rhoSigs = bindings ++ rhoSigs env})

-- Work with new predicate constants to the environment
withEnvPreds newCons polys =
    local (\env -> env{ polySigs = zip newCons polys ++ polySigs env})


{-
 - Other auxilliary TypeCheck functions
 -}


-- Find all subexpressions in a clause head

allHeadExprs :: SHead a -> [SExpr a]
allHeadExprs h = h |> headArgs 
                   |> concat 
                   |> concatMap flatten 
                 
-- Find all variables in a clause head 
allHeadVars h = h |> allHeadExprs
                  |> filter isVar
                  |> map ( \(SExpr_var _ v) -> nameOf v)


