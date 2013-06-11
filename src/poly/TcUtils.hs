--  Copyright (C) 2013 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--                     Emmanouil Koukoutos   <manoskouk@softlab.ntua.gr>
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

{-
 - Utility functions for Type checking
 -}

module TcUtils where

import Basic
import Types 
--import Parser
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
    deriving Show

lookupRho :: Eq a => a -> TyEnv a b -> Maybe RhoType
lookupRho a = (lookup a).rhoSigs

lookupPoly :: Eq b => b -> TyEnv a b -> Maybe PolyType
lookupPoly a = (lookup a).polySigs

-- The empty type checking environment
emptyTcEnv = TyEnv [] []

-- Concrete type environment
type PredSig = (Symbol, Int)
type TcEnv = TyEnv Symbol PredSig

-- Built-in predicates

builtins = [ ( (";",2::Int), Pi_fun 
                             [ Rho_pi phi
                             , Rho_pi phi
                             ] phi )
           , ( (",",2::Int), Pi_fun 
                             [ Rho_pi phi
                             , Rho_pi phi
                             ] phi )
           , ( ("=",2::Int), Pi_fun
                             [ Rho_i, Rho_i] Pi_o )
           ]          

    where phi = Pi_var $ Phi "phi"

builtins' = [(sig, generalize pi) | (sig, pi) <- builtins]

initTcEnv = TyEnv [] builtins'


-- A constraint is a pair of types with an associated expression. 
-- Convention: first type is the type syntactically assossiated with the expression
type Constraint a = (RhoType, RhoType, SExpr (Typed a))
    

-- Type Checker state
data TcState a = 
    TcState { uniq   :: Int             -- next fresh variable 
            , cnts   :: [Constraint a]  -- generated constraints     
            , exists :: [RhoSig Symbol] -- existentially quantified vars
            , msgs   :: Messages        -- error messages
            }

-- The empty state 
emptyTcState = TcState 1 [] [] ([],[])

-- TypeCheck monad. 
-- Supports environment, state, errors
type Tc m inf = ReaderT TcEnv (StateT (TcState inf) (ErrorT Messages m)) 


{-
 - Auxiliary functions for the Tc monad
 -}

-- Restrict an expression in the Head: no lambdas or predicate
-- constants allowed
restrictHead :: Monad m => SExpr a -> Tc m a ()
restrictHead h =  h |> flatten |> mapM_ restrict
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


-- Empty the state (except messages) to work in a new group
withEmptyState m = do
    modify (\st -> st{ uniq   = 1
                     , exists = []
                     , cnts   = []
                     }
           )
    local (\env -> env{rhoSigs = []}) m
 
-- Work with new variables in the environment
withEnvVars :: Monad m => [RhoSig Symbol] -> Tc m inf a -> Tc m inf a
withEnvVars bindings = 
    local (\env -> env{ rhoSigs = bindings ++ rhoSigs env})

-- Work with NO variable bindings in the environment
withNoEnvVars :: Monad m => Tc m inf a -> Tc m inf a
withNoEnvVars m = do
    modify ( \st  -> st {exists  = []} )
    local  ( \env -> env{rhoSigs = []} ) m

-- Work with new predicate constants to the environment
withEnvPreds bindings =
    local (\env -> env{ polySigs = bindings ++ polySigs env})
  


{-
 - Other auxilliary TypeCheck functions
 -}

-- Find all named variables in an expression
-- CAUTION: will contain a variable once for each of its appearances
allNamedVars expr = expr |> flatten 
                         |> filter isVar 
                         |> map ( \(SExpr_var _ v) -> nameOf v)
                         |> filter (/= "_")
                         

-- Fresh variable generation 
newAlpha :: Monad m => Tc m inf Alpha
newAlpha = do
    st <- get
    let n = uniq st
    put st{uniq = n+1}
    return $ Alpha ('a' : show n)

newPhi :: Monad m => Tc m inf Phi
newPhi = do
    st <- get
    let n = uniq st
    put st{uniq = n+1}
    return $ Phi ('t' : show n)

newAlphas n = replicateM n newAlpha
newPhis   n = replicateM n newPhi

-- Most general type of arity n as a pi-type
typeOfArity 0 = return Pi_o
typeOfArity n = do
    paramTypes <- newAlphas n
    resType    <- newPhi
    return $ Pi_fun (map Rho_var paramTypes) (Pi_var resType)

-- Generalize a predicate type
generalize pi = Poly_gen (freeAlphas $ Rho_pi pi) (freePhis $ Rho_pi pi) pi

-- Find free variables

freeAlphas (Rho_i)      = []
freeAlphas (Rho_var al) = [al]
freeAlphas (Rho_pi pi)  = aux pi
    where aux (Pi_fun rhos pi) = 
              nub $ aux pi ++ concatMap freeAlphas rhos
          aux _ = []

freePhis (Rho_pi pi) = aux pi
    where aux (Pi_o)           = []
          aux (Pi_var phi)     = [phi]
          aux (Pi_fun rhos pi) = 
              nub $ aux pi ++ concatMap freePhis rhos
freePhis _ = []

-- Freshen a polymoprhic type
freshen :: Monad m => PolyType -> Tc m inf PiType
freshen (Poly_gen alphas phis pi) = do
    alphas' <- newAlphas $ length alphas
    phis'   <- newPhis   $ length phis
    let ss = [ substAlpha alpha (Rho_var alpha') 
             | (alpha, alpha') <- zip alphas alphas'] ++
             [ substPhi phi (Pi_var phi') 
             | (phi, phi') <- zip phis phis']
    let s = foldl (.) id ss
    let Rho_pi pi' = s (Rho_pi pi)
    return pi'

{- 
 - Substitutions 
 -}

type Substitution = RhoType -> RhoType


-- Elementary substitution of an argument type variable with a type
-- substAlpha alpha rho rho' means substitute alpha with rho in rho'
substAlpha alpha rho rho'@(Rho_var alpha')
  | alpha == alpha'  =  rho
  | otherwise        =  rho'
substAlpha alpha rho (Rho_pi pi) = Rho_pi (aux alpha rho pi)
    where aux alpha rho (Pi_fun rhos pi) =
              Pi_fun (map (substAlpha alpha rho) rhos) (aux alpha rho pi)
          aux alpha rho pi = pi
substAlpha _ _ Rho_i = Rho_i

-- Elementary substitution of a predicate type variable with a type
substPhi phi pi (Rho_pi pi') = Rho_pi (aux phi pi pi')
  where aux phi pi pi'@(Pi_var phi') 
            | phi == phi' = pi
            | otherwise   = pi'
        aux phi pi (Pi_fun rhos pi') =
            Pi_fun (map (substPhi phi pi) rhos) (aux phi pi pi')
        aux _ _ Pi_o = Pi_o
substPhi phi pi rho = rho

-- Apply a substitution on a list of constraints
substCnts :: Substitution -> [Constraint a] -> [Constraint a]
substCnts s cstr = [(s rho1, s rho2, ex) | (rho1, rho2,ex) <- cstr]

