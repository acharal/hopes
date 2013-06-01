{-
 - Preprocess expressions before typecheck
 -}


module Prepr where

import Basic
import Types 
import Loc
import Syntax
import Parser
import Error
import TcUtils
import Data.List
import Data.Maybe (fromJust)
import Data.Graph

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
          

-- Infer arities and predicative status
fixSentence c@(SSent_comm _ _) = c
fixSentence (SSent_clause a' (SClause a hd bd) ) =
    SSent_clause a' $ SClause a (fixHead hd) $ 
        case bd of 
            Nothing          -> Nothing
            Just (gets, exp) -> Just ( gets, fixExpr True 0 exp )

fixHead (SHead a c givArr _ argList) = 
    -- if there are no arguments, infer arity 0
    let infArr = case argList of
                     [] -> 0
                     (hd:_) ->  length hd
    in SHead a c givArr infArr $ 
        map (map $ fixExpr False 0) argList

fixExpr :: Bool    -- interpred as predicate?
        -> Int     -- arity
        -> SExpr a -- examined expression
        -> SExpr a

fixExpr predSt ar ex = case ex of
    --SExpr_paren a ex1 -> 
    --    SExpr_paren a (fixExpr predSt ar ex1) 

    SExpr_const a con _ givArr _ ->
        SExpr_const a con predSt givArr ar 

    SExpr_var a v ->
        SExpr_var a v

    e@(SExpr_number _ _ ) ->
        e

    SExpr_predCon a c givArr _ ->
        SExpr_predCon a c givArr ar

    SExpr_app a func args ->
        SExpr_app a (fixExpr predSt ( length args) func) 
            -- pred. status goes to application functor
                    (map (fixExpr argPredSt 0) args)
        where argPredSt = predSt && isPredFunctor func
              isPredFunctor (SExpr_const _ (Const _ c) _ givArr _) =
                  elem (c,arr) predefSpecialPreds
                      where arr = case givArr of 
                                      Just n  -> n
                                      Nothing -> length args

    SExpr_op a op _ args ->
        SExpr_op a op predSt (map (fixExpr argPredSt 0) args)
        where argPredSt = predSt && isPredOp op ( length args)
              isPredOp (Const _ c) opArr = 
                  elem (c,opArr) predefSpecialPreds
                 
    SExpr_lam a vars bd ->
        SExpr_lam a vars (fixExpr True 0 bd)

    SExpr_list a initElems tl ->
        SExpr_list a (map (fixExpr False 0) initElems) 
            (fmap (fixExpr False 0) tl)
            -- fmap maps into Maybe 

    SExpr_eq a ex1 ex2 ->
        SExpr_eq a (fixExpr False 0 ex1) (fixExpr False 0 ex2)
   
    SExpr_ann a ex tp -> SExpr_ann a ex tp --TODO implement this!


-- Find all predicates with their arities defined in a clause
findReferredPreds :: SClause a -> [PredSig]
findReferredPreds clau = case clBody clau of 
    Nothing -> []
    Just (_,ex) -> map createPC $ filter isPredConst $ flatten ex
    where
        createPC ex = (nameOf ex, fromJust $ arity ex)

-- Find the predicate defined in a clause
findDefinedPred :: SClause a -> PredSig
findDefinedPred h = (nameOf $ clHead h, fromJust $ arity $ clHead h)
          

{-
 - Analyse dependencies between clauses and build dependency
 - groups
 - TODO : add predicates that are no nodes of the graph?
 -}

depAnalysis :: [SPredDef a] -> SDepGroupDag a
depAnalysis definitions = 
    let -- Find all referred predicates in a predicate definition
        allDeps def = nub $ concat $ map findReferredPreds (predDefClauses def) 
        -- create nodes in the form (node, key, [key])
        -- (see Data.Graph)
        nodes = map (\def -> ( def
                             , (nameOf def, fromJust $ arity def)
                             , allDeps def
                             )
                    ) definitions
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
    let sents'   = map fixSentence $ filter isClause sents
        clauses  = map (\(SSent_clause _ cl) -> cl) sents'
        clGroups = groupBy (\cl1 cl2 -> nameOf cl1 == nameOf cl2 && arity cl1 == arity cl2) clauses
        defs = map (\grp -> SPredDef { predDefName = nameOf $ head grp 
                                     , predDefArity = fromJust $ arity $ head grp
                                     , predDefClauses = grp
                                     }
                   ) clGroups 
    in depAnalysis defs


