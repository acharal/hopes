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
 
--predDefIndConsts = [ "[]" ]

--predDefPredConsts = [ "!" ]         

-- Infer arities and predicative status
fixSentence c@(SSent_comm _ _) = c
fixSentence (SSent_clause a' (SClause a hd bd) ) =
    SSent_clause a' $ SClause a (fixExpr True 0 hd) 
                    $ fmap (\(gets, exp) -> (gets, fixExpr True 0 exp)) bd

{-
fixHead (SHead a c givArr _ argList) = 
    -- if there are no arguments, infer arity 0
    let infArr = case argList of
                     [] -> 0
                     (hd:_) ->  length hd
    in SHead a c givArr infArr $ 
        map (map $ fixExpr False 0) argList
-}
fixExpr :: Bool    -- interpred as predicate?
        -> Int     -- arity
        -> SExpr a -- examined expression
        -> SExpr a

fixExpr predSt ar ex = case ex of
    --SExpr_paren a ex1 -> 
    --    SExpr_paren a (fixExpr predSt ar ex1) 

    -- List is never predicate (don't need this)
    SExpr_const a con _ givArr _ ->
        SExpr_const a con (predSt {-&& not (nameOf con `elem` predDefIndConsts)-}) givArr ar 

    SExpr_var a v ->
        SExpr_var a v

    e@(SExpr_number _ _ ) ->
        e

    SExpr_predCon a c givArr _ ->
        SExpr_predCon a c givArr ar

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

    SExpr_op a op _ args ->
        SExpr_op a op predSt (map (fixExpr argPredSt 0) args)
        where argPredSt = predSt && (nameOf op, length args) `elem` predefSpecialPreds --isPredOp op ( length args)
              --isPredOp (Const _ c) opArr = 
              --    elem (c,opArr) predefSpecialPreds
                 
    SExpr_lam a vars bd ->
        SExpr_lam a vars (fixExpr True 0 bd)

    SExpr_list a initElems tl ->
        SExpr_list a (map (fixExpr False 0) initElems) 
            (fmap (fixExpr False 0) tl)
            -- fmap maps into Maybe 

    --SExpr_eq a ex1 ex2 ->
    --    SExpr_eq a (fixExpr False 0 ex1) (fixExpr False 0 ex2)
   
    SExpr_ann a ex tp -> SExpr_ann a ex tp --TODO implement this!


-- Find all predicates with their arities defined in a clause
-- body
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
        nodes = map (\def -> ( def
                             , (predDefName def, predDefArity def)
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
    let cldefs = sents  -- Keep only clauses
                        |> filter isClause 
                        -- Fix arities and pred. statuses
                        |> map fixSentence 
                        -- Keep (def, clause) pairs, with def the defined predicate
                        |> map (\(SSent_clause _ cl) -> (findDefinedPred cl,cl))
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

{-
        clGroups = groupBy (\cl1 cl2 -> nameOf cl1 == nameOf cl2 && arity cl1 == arity cl2) clauses
        defs = map (\grp -> SPredDef { predDefName = nameOf $ head grp 
                                     , predDefArity = fromJust $ arity $ head grp
                                     , predDefClauses = grp
                                     }
                   ) clGroups 
    in depAnalysis defs
-}

