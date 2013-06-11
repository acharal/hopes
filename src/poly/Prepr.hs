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
 - Preprocess expressions before typecheck
 -}


module Prepr where

import Basic
import Types 
import Syntax
import Parser
import Error
import TcUtils
import Data.List
import Data.Maybe (fromJust)
import Data.Graph
import Data.Function(on)
import Control.Monad.Reader
import Control.Monad.State


{-
 - Expression preprocessing
 -}

-- Operators/ predicates which pass their predicative
-- status down to their operands
predefSpecialPreds = [ ("->"  , 2)
                     , (","   , 2)
                     , (";"   , 2)
                     , ("\\+" , 1)
                     --, ("call", 1)
                     --, ("once", 1)
                     --, ("findall", 1)
                     ]
 

-- Infer arities and predicative status in syntactic structures

-- TODO implement commands differently?
fixSentence (SSent_comm (SCommand a ex)) = 
    SSent_comm $ SCommand a $ fixExpr True 0 ex
fixSentence (SSent_clause (SClause a hd bd) ) =
    SSent_clause $ SClause a (fixExpr True 0 hd) 
                 $ fmap ( \(gets, ex) -> (gets, fixExpr True 0 ex) ) bd

fixExpr :: Bool    -- interpred as predicate? (predicate status)
        -> Int     -- arguments FOLLOWING, so arity of this expression
        -> SExpr a -- examined expression
        -> SExpr a -- fixed expression

fixExpr predSt ar ex = case ex of
    
    -- Arity and predicate status are passed from environment
    SExpr_const a con _ givArr _ ->
        SExpr_const a con predSt givArr ar 
    
    -- Variables and numbers remain unchanged
    e@(SExpr_var a v) ->
        e

    e@(SExpr_number _ _ ) ->
        e

    -- Predicate constants are passed their arity
    SExpr_predCon a c givArr _ ->
        SExpr_predCon a c givArr ar

    -- Application
    SExpr_app a func args ->
        SExpr_app a func' args'        
        where -- pred. status goes to application functor
              func' = fixExpr predSt ( length args) func
              -- args are predicate if func' is a predicate constant AND
              -- a special constant
              args' = map (fixExpr argPredSt 0) args
              argPredSt = isPredConst func' && isSpecialConst func'
              isSpecialConst func = 
                  (nameOf func, fromJust $ arity func) `elem` predefSpecialPreds

    -- Similarly to above, only more concise
    SExpr_op a op _ args ->
        SExpr_op a op predSt (map (fixExpr argPredSt 0) args)
        where argPredSt = predSt && (nameOf op, length args) `elem` predefSpecialPreds 
     
    -- Body of a lambda abstraction in a predicate            
    SExpr_lam a vars bd ->
        SExpr_lam a vars (fixExpr True 0 bd)

    -- List elements are no predicates 
    SExpr_list a initElems tl ->
        SExpr_list a (map (fixExpr False 0) initElems) 
            (fmap (fixExpr False 0) tl) -- fmap maps into Maybe 
    
    
    SExpr_ann a ex' tp -> SExpr_ann a (fixExpr predSt ar ex') tp --TODO implement this!


-- Find all predicates with their arities defined in a clause body
findReferredPreds :: SClause a -> [PredSig]
findReferredPreds clau = case clBody clau of 
    Nothing -> []
    Just (_,ex) -> ex |> flatten |> filter hasPredConst |> map createPC
    where
        createPC ex = (nameOf ex, fromJust $ arity ex)

-- Find the predicate defined in a clause
findDefinedPred :: SClause a -> PredSig
findDefinedPred h = walk $ clHead h 
    where walk ex@(SExpr_const _ c _ _ _) = (nameOf c, fromJust $ arity ex)
          walk (SExpr_app _ ex' _ ) = walk ex'
          walk _ = error "Internal: Illegal head structure" 

          

{-
 - Analyse dependencies between clauses and build dependency
 - groups
 - TODO : add predicates that are no nodes of the graph?
 -}

depAnalysis :: [SPredDef a] -> SDepGroupDag a
depAnalysis definitions = 
    let -- Find all referred predicates in a predicate definition
        allDeps def = nub $ concatMap findReferredPreds (predDefClauses def) 
        -- create nodes in the form (node, key, [key])
        -- (see Data.Graph)
        nodes = [(def, (predDefName def, predDefArity def), allDeps def)
                | def <- definitions
                ]  
        -- Find strongly connected components
        depGroups = stronglyConnComp nodes
    in map flattenSCC depGroups


{-
 - Putting it all together:
 - From a list of sentences to the DAG of all predicate
 - definitions which will be fed to the type checker
 -}

progToGroupDag :: [SSent a] -> SDepGroupDag a
progToGroupDag sents = 
    let cldefs = sents  -- Keep only clauses
                        |> filter isClause 
                        -- Fix arities and pred. statuses
                        |> map fixSentence 
                        -- Keep (def, clause) pairs, with def the defined predicate
                        |> map (\(SSent_clause cl) -> (findDefinedPred cl,cl))
                        -- Group by which predicate is defined
                        |> sortBy  (compare `on` fst)
                        |> groupBy (\cl1 cl2 -> fst cl1 == fst cl2)
                        -- Now from each group, keep defined name and arity, 
                        -- and respective clauses
                        |> map (\grp -> SPredDef { predDefName = fst $ fst $ head grp
                                                 , predDefArity= snd $ fst $ head grp
                                                 , predDefClauses = map snd grp
                                                 }
                               )
    -- Feed the definitions into dependency analysis
    in depAnalysis cldefs

