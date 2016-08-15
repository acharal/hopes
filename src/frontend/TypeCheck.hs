--  Copyright (C) 2013 Angelos Charalambidis  <a.charalambidis@di.uoa.gr>
--                     Emmanouil Koukoutos    <manoskouk@softlab.ntua.gr>
--                     Nikolaos S. Papaspyrou <nickie@softlab.ntua.gr>
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

module TypeCheck (
    runTcT
  , initTcEnv
  , TcOutput(..)
  , tcProg
  , tcGoal
  , withEnvPreds
  , PolySig
  , PredSig
  , Tc
  ) where

import Basic
import TcUtils
import Syntax
import Types
import Error (Messages, internalErr, mkMsgs, mkErrWithLoc, ErrType(..), ErrLevel(..) )
import Loc (HasLocation(..))
import Pretty

import Data.Maybe(fromJust)
import Data.List(group, sort)

{-
 - Type check a program
   - Create constraints
   - Add type annotations to syntax tree
 - Structures broader than expressions have type o by
 -   convention
 -}

-- A data type for all relevant information the type checking outputs
data TcOutput a =
    TcOutput { -- annotated program
               tcOutSyntax   :: SProg (Typed a)
               -- new environment predicates
             , tcOutPreds    :: [PolySig PredSig]
               -- Warnings
             , tcOutWarnings :: Messages
             }


-- run the typeCheck monad with a starting environment
runTc :: (Show a, HasLocation a, Monad m) => TcEnv -> SDepGroupDag a -> m (Either Messages (TcOutput a))
runTc env dag = runTcT env (tcProg prog)
  where prog = SProg { progDefs = dag, progCommands = [], progGoals = [] }


tcProg :: (Show a, HasLocation a, Monad m) => SProg a -> Tc a m (TcOutput a)
tcProg prog = do
     (defs', preds) <- walk (progDefs prog) []

     let rho_o = Rho_pi Pi_o

     commands' <- withEnvPreds preds $ withEmptyState $
         mapM tcCommand (progCommands prog)

     goals' <- withEnvPreds preds $ withEmptyState $
         mapM tcGoal (progGoals prog)

     warnings <- gets msgs
     return $ TcOutput (SProg { progDefs = defs', progCommands = commands', progGoals = goals'}) preds warnings
   where -- reached the end of the dag, so return all preds. you found
         walk [] preds = return ([], preds)
         walk (grp:grps) preds = do
             -- TypeCheck current group with current predicates
             (grp' , preds' ) <- withEnvPreds preds $ withEmptyState (tcGroup grp)
             -- Continue with the extra you found in this group
             (grps', preds'') <- walk grps (preds' ++ preds)
             -- return all groups and all predicates
             return (grp':grps', preds'')

{- typeCheck program, and output all relevant information
tcDag :: (Show a, HasLocation a, Monad m) => SDepGroupDag a -> Tc m a (TcOutput a)
tcDag dag = do
    (dag', preds) <- walk dag []
    warnings      <- gets msgs
    return $ TcOutput dag' preds warnings

    where -- reached the end of the dag, so return all preds. you found
          walk [] preds = return ([], preds)
          walk (grp:grps) preds = do
              -- TypeCheck current group with current predicates
              (grp' , preds' ) <- withEnvPreds preds $ withEmptyState (tcGroup grp)
              -- Continue with the extra you found in this group
              (grps', preds'') <- walk grps (preds' ++ preds)
              -- return all groups and all predicates
              return (grp':grps', preds'')
-}


-- typeCheck a dependency group
tcGroup group = do
    -- Find predicates to be defined in this group
    let preds = [(predDefName pr, predDefArity pr) | pr <- group ]
    -- Make the most general types
    types <- mapM (typeOfArity.snd) preds
    let polys = map piToPoly types
    -- typeCheck group with the new predicates in the env.
    -- and no other state
    group' <- withEnvPreds (zip preds polys) (mapM tcPredDef group)
    -- Get the created constraints from the state and unify
    stCons <- gets cnts
    subst  <- unify stCons
    let -- Substitute in type annotations in group
        groupSub = map (fmap $ substInTyped subst) group'
        -- ... and in new predicate types
        typesSub = map (substInPi subst) types
    -- Return annotated group along with new predicate signatures
    return (groupSub, zip preds $ map generalize typesSub)
    where substInTyped subst typed =
              let newType = typed |> typeOf |> subst in
              hasType newType typed
          substInPi subst pi = pi'
              where Rho_pi pi' = subst $ Rho_pi pi

-- typeCheck a predicate definition
tcPredDef predDef = do
    -- Begin with new variable bindings for each clause
    clauses' <- mapM (withNoEnvVars.tcClause) (predDefClauses predDef)
    return $ predDef{predDefClauses = clauses'}

tcGoal (SGoal a e) =
  let rho_o = Rho_pi Pi_o
  in do
    e' <- tcExpr e
    addConstraint (typeOf e') rho_o e'
    stCons <- gets cnts
    subst  <- unify stCons
    return $ SGoal (typed rho_o a) (fmap (substInTyped subst) e')
  where substInTyped subst typed =
              let newType = typed |> typeOf |> subst in
              hasType newType typed
        substInPi subst pi = pi'
              where Rho_pi pi' = subst $ Rho_pi pi

tcCommand (SCommand a e) =
  let rho_o = Rho_pi Pi_o
  in do
    e' <- tcExpr e
    addConstraint (typeOf e') rho_o e'
    return $ SCommand (typed rho_o a) e'

-- typeCheck a clause
tcClause (SClause a hd bd) = do
    -- Make sure head contains no illegal expressions
    restrictHead hd
    -- Find all variables in the head and group them
    let vars = allNamedVars hd |> sort |> group
    alphas <- newAlphas (length vars)
    --let varTypes = map Rho_var alphas
    -- More than 1 appearances of a variable means it has type i
    let varsWithTypes =
            [ if length var > 1 then (head var, Rho_i) else (head var, tp)
            | (var, tp) <- zip vars (map Rho_var alphas)
            ]

    hd' <- withEnvVars varsWithTypes (tcExpr hd)
    let rho_o = Rho_pi Pi_o
    case bd of
        Just (gets, expr) -> do
            expr' <- withEnvVars varsWithTypes (tcExpr expr)
            case gets of
                SGets_mono -> do
                    -- Both head and body are booleans
                    addConstraint (typeOf hd') rho_o hd'
                    addConstraint (typeOf expr') rho_o expr'
                    return $ SClause (typed rho_o a) hd' $ Just (gets, expr')
                SGets_poly -> do
                    -- head and body must have the same type
                    addConstraint (typeOf expr') (typeOf hd') expr'
                    return $ SClause (typed rho_o a) hd' $ Just (gets, expr')
        Nothing -> do
            -- No body implies a true constant as expression,
            -- so type is o
            addConstraint (typeOf hd') rho_o hd'
            return $ SClause (typed rho_o a) hd' Nothing



-- typeCheck an expression.
--   Also mark ex. quantified variables the first time you see them
tcExpr :: Monad m => SExpr a
                  -> Tc a m ( SExpr (Typed a) )

-- 1) Individuals
-- Number
tcExpr (SExpr_number a num) =
    return $ SExpr_number (typed Rho_i a) num

-- Individual constant
tcExpr ex@(SExpr_const _ _ False _ _) =
    return $ fmap (typed Rho_i) ex
    -- fmap conveniently maps content to a typed content


-- 2) Predicate constant
-- Predicate constant, first case
tcExpr ex@(SExpr_const _ c True _ _) = do
    let ar = fromJust $ arity ex
    tp <- findPoly (nameOf c) ar
    return $ fmap (typed tp) ex

-- Predicate constant, second case
tcExpr ex@(SExpr_predCon _ c _ _) = do
    let ar = fromJust $ arity ex
    tp <- findPoly (nameOf c) ar
    return $ fmap (typed tp) ex


-- 3) Variable

-- Named variable
tcExpr ex@(SExpr_var a var@(Var _ _) isEx) = do
    -- search in environment
    envTp <- asks $ lookupRho $ nameOf var
    tp <- case envTp of
        tp'@(Just _) -> return tp'
        Nothing  -> do
            -- If not found, search in exist. vars
            exBound <- gets exists
            return $ lookup (nameOf var) exBound
    case tp of
        -- Found var in env+state
        Just tp' -> return $ SExpr_var (typed tp' a)
                                      (fmap (typed tp') var)
                                      False
        -- Did not find, so add it to the state
        Nothing -> do
            al <- newAlpha
            let newRho = Rho_var al
            addExist (nameOf var) newRho
            return $ SExpr_var (typed newRho a)
                               (fmap (typed newRho) var)
                               True -- Mark as exist. quantified

-- Anonymous variable
tcExpr ex@(SExpr_var a (AnonVar vinf) _) = do
    -- Anonymous variable has fresh type
    alpha <- newAlpha
    return $ fmap (typed $ Rho_var alpha) ex

-- 4) Functional app.
-- Application, functional
tcExpr ex@(SExpr_app a fun@(SExpr_const _ _ False _ _) args) = do
    -- Functor is a functional
    let fun' = fmap (typed Rho_i) fun -- TODO: UGLY, using i type as sigma
    -- typecheck args
    args' <- mapM tcExpr args
    -- Args must have type i
    mapM_ (\ex -> addConstraint (typeOf ex) Rho_i ex) args'
    return $ SExpr_app (typed Rho_i a) fun' args'


-- Operator, functional
tcExpr (SExpr_op a c prec False args) = do
    let c' = fmap (typed Rho_i) c -- TODO: UGLY, using i type as sigma
    -- typecheck args
    args' <- mapM tcExpr args
    -- Args must have type i
    mapM_ (\ex -> addConstraint (typeOf ex) Rho_i ex) args'
    return $ SExpr_op (typed Rho_i a) c' prec False args'

-- List
tcExpr (SExpr_list a hds tl) = do
    hds' <- mapM tcExpr hds
    -- initial elements are of type i
    mapM_ (\ex -> addConstraint (typeOf ex) Rho_i ex) hds'
    case tl of
        Just tl'' -> do
            -- Tail is also of type i
            tl' <- tcExpr tl''
            addConstraint (typeOf tl') Rho_i tl'
            return $ SExpr_list (typed Rho_i a) hds' (Just tl')
        Nothing ->
            return $ SExpr_list (typed Rho_i a) hds' Nothing


-- 5) Predicate application
-- Application, predicate
tcExpr (SExpr_app a func args) = do
    func' <- tcExpr func
    args' <- mapM tcExpr args
    phi   <- newPhi
    let tp = Rho_pi $ Pi_fun (map typeOf args') (Pi_var phi)
        funcTp = typeOf func'
    addConstraint funcTp tp func'
    return $ SExpr_app (typed (Rho_pi $ Pi_var phi) a) func' args'

-- Operator, predicate
tcExpr (SExpr_op a c@(Const cinf cnm) prec True args) = do
    headTp <- findPoly cnm (length args)
    args'  <- mapM tcExpr args
    phi    <- newPhi
    let tp = Rho_pi $ Pi_fun (map typeOf args') (Pi_var phi)
        c' = fmap (typed headTp) c
        a' = typed (Rho_pi $ Pi_var phi) a
        cinf' = typed headTp cinf
    -- To add constraint, create a 'ghost' constant expression
    -- with the correct information of the operator
    addConstraint headTp tp ( SExpr_const cinf' (Const cinf' cnm) True Nothing (length args) )
    return $ SExpr_op a' c' prec True args'

-- 6) Rest
-- Lambda abstraction
tcExpr (SExpr_lam a vars bd) = do
    alphas <- newAlphas (length vars)
    -- Fresh variable types
    let argTypes = map Rho_var alphas
    -- Result type
    phi <- newPhi
    let varNames = map nameOf vars
    -- Bindings to pass down to body as extra env. Only named
    -- vars are needed
    let bindings = zip varNames argTypes |> filter(\x -> fst x /= "_")
    -- Put variables in the environment with fresh types
    -- and typeCheck body
    bd' <- withEnvVars bindings (tcExpr bd)
    let bdType = typeOf bd'
    addConstraint bdType (Rho_pi $ Pi_var phi) bd'
    let -- Type of the whole expression
        lamType = Rho_pi $ Pi_fun argTypes (Pi_var phi)
        -- Variables with their types
        vars'   = [ fmap (typed tp) var
                  | (var,tp) <- zip vars argTypes
                  ]
    return $ SExpr_lam (typed lamType a) vars' bd'

-- Type annotated
tcExpr (SExpr_ann _ _ _) = throwError $ mkMsgs $ internalErr $ text "annotations not implemented yet"

-- In parens
tcExpr (SExpr_paren a ex) = do
    ex' <- tcExpr ex
    return $ SExpr_paren (getInfo ex') ex'

-- Utility function to search environment for expr.
findPoly :: Monad m => Symbol -> Int -> Tc a m RhoType
findPoly cnm ar = do
    envTp <- asks $ lookupPoly (cnm,ar)
    tp <- case envTp of
        Nothing -> do -- Type is left free
            pi <- typeOfArity ar
            return $ Rho_pi pi
        Just envTp' -> do
            pi <- freshen envTp'
            return $ Rho_pi pi
    return tp

-- Unification
unify :: (Monad m, Show a, HasLocation a) => [Constraint a] -> Tc a m Substitution
-- No constraints
unify [] = return id
-- Equal types
unify ( (rho1, rho2, _) : tl) | rho1 == rho2 = unify tl
-- Argument variable
unify ( (Rho_var alpha, rho2, _) : tl)
    | not $ alpha `elem` freeAlphas rho2 = do
        let subst = substAlpha alpha rho2
        subst' <- unify (substCnts subst tl)
        return $ subst' . subst
unify ( (rho1, Rho_var alpha, _) : tl)
    | not $ alpha `elem` freeAlphas rho1 = do
        let subst = substAlpha alpha rho1
        subst' <- unify (substCnts subst tl)
        return $ subst' . subst
-- Predicate variable
unify ( (Rho_pi (Pi_var phi), rho2@(Rho_pi pi), _) : tl )
    | not $ phi `elem` freePhis rho2 = do
        let subst = substPhi phi pi
        subst' <- unify (substCnts subst tl)
        return $ subst' . subst
unify ( (rho1@(Rho_pi pi), Rho_pi (Pi_var phi), _) : tl )
    | not $ phi `elem` freePhis rho1 = do
        let subst = substPhi phi pi
        subst' <- unify (substCnts subst tl)
        return $ subst' . subst
-- Predicate function
unify ( ( f1@(Rho_pi (Pi_fun rhos1 pi1)), f2@(Rho_pi (Pi_fun rhos2 pi2)), ex ) : tl)
    | length rhos1 == length rhos2 = do

        -- Try to unify subtypes of these types
        partial <- unify ( (Rho_pi pi1, Rho_pi pi2, ex) :
                           zip3 rhos1 rhos2 (replicate (length rhos1) ex)
                         )
                   -- if you catch something, throw a better message
                   -- for the whole type
                   `catchError` (\_ -> throwError $ unificationError f1 f2 ex)
        subst' <- unify (substCnts partial tl)
        return $ subst' . partial

        {- Something simpler that is more likely to work -}{-
        unify $ ( zip3 rhos1 rhos2 (replicate (length rhos1) ex) ) ++
                [( Rho_pi pi1, Rho_pi pi2, ex )] ++
                tl  -}

-- Everything else is unification error
unify ((rho1, rho2, ex) : _ ) =
    throwError $ unificationError rho1 rho2 ex

unificationError :: (Show a, HasLocation a)
                 => RhoType -> RhoType -> SExpr a -> Messages
unificationError rho1 rho2 ex =
    mkMsgs $ mkErrWithLoc (locSpan ex) TypeError Fatal $
              (text "Type Error: Could not match expected type" <+> (quotes $ ppr rho2) ) $$
              (text "with actual type" <+> (quotes $ ppr rho1) ) $$
              (text "in expression" <+> (doubleQuotes $ ppr ex) )

{- another version
             (nest 4 $ text "type error: expression" <+> ppr ex) $$
             (nest 4 $ text "has type:" <+> ppr rho1) $$
             (nest 4 $ text "but was exprected with type:" <+> ppr rho2)
-}
