module TcUtils where

import Types 
import Loc
import Syntax
import Data.List

{-
 - In polyHOPES, some constants are interpreted as arguments
 - and some as individual/functional symbols
 -}

-- Operators/ predicates which pass their predicative
-- status down to their operands
predefPredFunc = [ ("->"  , 2)
                 , (","   , 2)
                 , (";"   , 2)
                 , ("\\+" , 1)
                 , ("call", 1)
                 , ("once", 1)
                 , ("findall", 1)
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
    SExpr_paren a ex1 -> 
        SExpr_paren a (fixExpr predSt ar ex1) 

    SExpr_const a con _ givArr _ ->
        SExpr_const a con predSt givArr ar 

    SExpr_var a v ->
        SExpr_var a v

    SExpr_int a i ->
        SExpr_int a i 

    SExpr_float a f ->
        SExpr_float a f 

    SExpr_predCon a c givArr _ ->
        SExpr_predCon a c givArr ar

    SExpr_app a func args ->
        SExpr_app a (fixExpr predSt ( length args) func) 
            -- pred. status goes to application functor
                    (map (fixExpr argPredSt 0) args)
        where argPredSt = predSt && isPredFunctor func
              isPredFunctor (SExpr_const _ (Const _ c) _ givArr _) =
                  elem (c,arr) predefPredFunc
                      where arr = case givArr of 
                                      Just n  -> n
                                      Nothing ->  length args

    SExpr_op a op _ args ->
        SExpr_op a op predSt (map (fixExpr argPredSt 0) args)
        where argPredSt = predSt && isPredOp op ( length args)
              isPredOp (Const _ c) opArr = 
                  elem (c,opArr) predefPredFunc
                 
    SExpr_lam a vars bd ->
        SExpr_lam a vars (fixExpr True 0 bd)

    SExpr_list a initElems tl ->
        SExpr_list a (map (fixExpr False 0) initElems) 
            (fmap (fixExpr False 0) tl)
            -- fmap maps into Maybe 

    SExpr_eq a ex1 ex2 ->
        SExpr_eq a (fixExpr False 0 ex1) (fixExpr False 0 ex2)
   
    SExpr_ann a ex tp -> SExpr_ann a ex tp --TODO implement this!







{-



depAnalysis :: [SSentence (Typed a)] -> SGroups a
depAnalysis sentList = 
    let clauses = 

-}
