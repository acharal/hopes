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
 - Desugarer module:
 - Transforms surface language constructs (S...)
 -     to core language constructs (C...)
 -     or polyHOPES constructs to polyH constructs
 -
 - Output language is used by the prover
 -}

module Desugar where


import Syntax
import Types
import Core hiding (isVar)
import Basic
import TcUtils (generalize)

import Data.Maybe(fromJust, mapMaybe)
import Data.List (nub, (\\))
import Control.Monad.State

desugarProg prog =
  let defs = concatMap desugarDag (progDefs prog)
      goals = map (\(SGoal _ e) -> desugarExpr e) (progGoals prog)
      commands = map (\(SCommand _ e) -> desugarExpr e) (progCommands prog)
  in (defs, commands, goals)

desugarGoal (SGoal inf goal) = desugarExpr goal

-- Desugar the dependency group DAG
desugarDag dag = map desugarDef dag

-- Desugar a dependency group
desugarGroup group = map desugarDef group

-- Desugar a predicate definition
desugarDef def =
    CPredDef { cPredName = predDefName def
             , cPredAr   = predDefArity def
             , cPredTp   = def |> predDefClauses
                               |> head
                               |> clInfo
                               |> typeOf
                               |> (\(Rho_pi pi) -> generalize pi)
             , cPredIsMono = any isMonoCl (predDefClauses def)
             , cPredCls    = map desugarClause (predDefClauses def)
             }
    where isMonoCl (SClause _ _ (Just (SGets_poly, _ )) ) = False
          isMonoCl _ = True

-- Desugar a clause. This is the tricky part of desugaring.
--     Make head parameters into lambda abstractions
--     Introduce new variables in these abstractions where you find
--         duplicate variables or functional applications
--     Whenever you do, also introduce an extra unification in the
--         body.
--     If body is still empty, put CTrue in its place.
--     Add existential quantifiers where necessary.
desugarClause (SClause inf hd bd) =
    let findHeadArgs (SExpr_app _ func args) =
            findHeadArgs func ++ [args]
        findHeadArgs _ = []
        args = findHeadArgs hd
        (args', finalSt) = runArgMonad $ mapM ( mapM desugarArg ) args
        -- Additional unifications
        finalPairs = pairs finalSt
        -- desugar body
        bd' = case bd of
                  Nothing -> CTrue
                  Just (_, ex) -> desugarExpr ex
        finalBd = case (finalPairs, bd') of
            ([], _) -> bd'

            -- We have to add extra unifications to the body

            -- Don't keep unneeded CTrue
            (_ , CTrue) -> foldedPairs
            -- Keep body
            (_, _ ) -> case typeOf bd' of
                Rho_pi Pi_o -> CAnd (Rho_pi Pi_o) foldedPairs bd'
                -- In case of type mismatch, lift unif. to fit body type
                pi -> CAnd pi (CLift pi foldedPairs) bd'
            where foldedPairs = foldl1 (CAnd $ Rho_pi Pi_o) finalPairs


        -- Find variables to quantify
        fromExVar (SExpr_var _ (Var a v) True) =
            Just $ Flex (typeOf a) v
        fromExVar _ =
            Nothing
        -- In the body, variables to quantify are already marked
        exVarsBd = case bd of
                       Nothing -> []
                       Just (_, ex) -> ex |> flatten
                                          |> filter isVar
                                          |> mapMaybe fromExVar
        -- We created some extra quantifiable vars.
        exVarsArgs = concatMap allCVars finalPairs \\ concat args'
        exVars = nub $ exVarsArgs ++ exVarsBd
        -- Body with existentials
        exBody = foldr (\vr bd -> CExists (typeOf bd) vr bd) finalBd exVars
        -- Create the type of a lambda abstraction
        lType args bd = Rho_pi $ Pi_fun (map typeOf args) piBd
            where (Rho_pi piBd) = typeOf bd
    in  foldr (\args bd -> CLambda (lType args bd) args bd) exBody args'

-- Monadic treatment of head desugaring

-- The state of head desugaring
data ArgState = ArgState { uniq  :: Int      -- fresh variable no.
                         , seen  :: [Symbol] -- vars. we have already seen ALONE in an argument
                         , pairs :: [CExpr]  -- Additional unifications
                         }
type ArgMonad = State ArgState

-- Returns true if we have already encountered nm
isSeen :: VarSym -> ArgMonad Bool
isSeen nm = do
    sn <- gets seen
    return $ nm `elem` sn

-- Return a fresh variable name
newVar :: ArgMonad VarSym
newVar = do
    st <- get
    let un = uniq st
    put st{uniq = un + 1}
    return $ "__" ++ show un

-- Add a variable-expression pair to be unified in the state
addPair var expr =
    modify $ \st -> st{pairs = (CEq var expr) : pairs st}

-- Add a variable to the list of already encountered variables
addSeen var =
    modify $ \st -> st{seen = var : seen st}

-- Treat a clause argument
desugarArg :: HasType a
           => SExpr a
           -> ArgMonad (Flex)

-- Arg is a named variable
desugarArg (SExpr_var inf (Var vinf var) _) = do
    sn <- isSeen var
    if sn then do -- Add a unification to the final clause
                  nv <- newVar
                  let flex = Flex (typeOf vinf) nv
                  addPair (CVar $ Flex (typeOf vinf) var) (CVar flex)
                  return flex
          else do -- Add variable to the list of encountered vars.
                  addSeen var
                  return $ Flex (typeOf inf) var

-- Anonymous variables need nothing extra
desugarArg (SExpr_var inf (AnonVar vinf) _) =
    return $ AnonFlex (typeOf vinf)

-- Any other expression at this point is a functional application
-- and we have to add a unification clause to the final clause
desugarArg expr = do
    let expr' = desugarExpr expr
    nv <- newVar
    let flex = Flex Rho_i nv
    addPair (CVar flex) expr'
    return flex

-- Run SubstMonad from an empty state
runArgMonad m = runState m emptyArgState
    where emptyArgState = ArgState 1 [] []


-- Desugar an expression
desugarExpr :: HasType a => SExpr a -> CExpr

-- Individual constant, [] is taken into account here
desugarExpr ex@(SExpr_const a c False _ _) =
    if nameOf c == "[]" then CNil else CConst $ nameOf c
-- Predicate constant, first case
desugarExpr ex@(SExpr_const a c True _ _) =
    transformPConst (typeOf a) ((nameOf c),(fromJust $ arity ex))

-- Variable
desugarExpr (SExpr_var a var _) =
    CVar (varToFlex var)

-- Number
desugarExpr (SExpr_number a num) =
    CNumber num

-- Predicate constant, second case
desugarExpr ex@(SExpr_predCon a c _ _) =
    transformPConst (typeOf a) ((nameOf c),(fromJust $ arity ex))


-- Application, functional. '.' is taken into account here
-- TODO allow '.' as a functor
desugarExpr (SExpr_app a func args)
    | typeOf a == Rho_i =
        if nameOf func == "." && length args == 2
            then CCons (desugarExpr $ head args)
                       (desugarExpr $ head $ tail args)
            else CApp ( Rho_i )
                      ( CConst (nameOf func) )
                      ( map desugarExpr args )
-- Application, predicate
desugarExpr (SExpr_app a func args) =
    transformCApp (typeOf a)
                  (desugarExpr func)
                  (map desugarExpr args)

-- Operator, functional
desugarExpr (SExpr_op a c _ False args) =
    CApp ( Rho_i )
         ( CConst (nameOf c) )
         ( map desugarExpr args )
-- Operator, predicate
desugarExpr (SExpr_op a c _ True args) =
    transformCApp (typeOf a)
                  (CPred (typeOf c) ((nameOf c),(length args)))
                  (map desugarExpr args)

-- Lambda abstraction
desugarExpr (SExpr_lam a vars bd) = do
    CLambda (typeOf a) (map varToFlex vars) (desugarExpr bd)

-- List
desugarExpr (SExpr_list a hds tl) =
    foldr (CCons)
          (desugarTail tl)
          (map desugarExpr hds)
    where desugarTail Nothing   = CNil
          desugarTail (Just ex) = desugarExpr ex

-- Type annotated
desugarExpr (SExpr_ann _ _ _) = error "annotations not implemented yet"

-- In parens
desugarExpr (SExpr_paren _ ex) = desugarExpr ex


-- Transform Var to Flex
varToFlex (Var a nm)  = Flex (typeOf a) nm
varToFlex (AnonVar a) = AnonFlex (typeOf a)

-- Transform predicate constants
--    "true", "fail" and "!" are transformed to the structures
--        describing their special meaning
--    ";", "," and "=" are transformed to equivalent,
--        non-built-in predicates, to be passed as parameters
transformPConst tp nm = case lookup nm corePreds of
    Just f  -> f tp
    Nothing -> CPred tp nm

corePreds = [ ( ("true", 0) , \_  -> CTrue )
            , ( ("fail", 0) , \_  -> CFail )
            , ( ("!"   , 0) , \_  -> CCut  )
            , ( (","   , 2) , \tp -> CPred tp   ("and",2))
            , ( (";"   , 2) , \tp -> CPred tp   ("or",2))
            , ( ("="   , 2) , \_  -> CPred eqTp ("eq",2))
            ]
    where eqTp = Rho_pi $ Pi_fun [Rho_i, Rho_i] Pi_o

-- Transform application
--     If the functor is the ",", "," or "=" predicate,
--     the expression is transformed to the to the structure
--         describing its special meaning


transformCApp tp func@(CPred _ nm) args =
    case lookup nm coreApps of
        Just f  -> f tp args
        Nothing -> CApp tp func args
transformCApp tp func args =
    CApp tp func args

coreApps = [ ( (",",2) , \tp [ex1,ex2] -> CAnd tp ex1 ex2 )
           , ( (";",2) , \tp [ex1,ex2] -> COr  tp ex1 ex2 )
           , ( ("=",2) , \_  [ex1,ex2] -> CEq     ex1 ex2 )
           ]
