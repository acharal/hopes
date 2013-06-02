module TypeCheck where

import Basic
import Prepr
import TcUtils
import Syntax
import Types
import Parser
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe(fromJust)


{-
 - Type check a program
   - Create constraints
   - Add type annotations to syntax tree
 -}

{-
tcProgram :: SDepGroupDag a    -- program
          -> TcEnv             -- initial env.
          -> (SDepGroupDag (Typed a), TcEnv) 
             -- type annotated program along with final env.

tcProgram groups env = 
    sequence $ TcEnv

{-tcGroup :: SDepGroup a -- group
        -> TcEnv       -- initial env.
        -> (SDepGroup (Typed a), TcEnv) 
           -- type annotated program along with final env.
-}
tcGroup :: Monad m => SDepGroup a -> Tc m (SDepGroup (Typed a))
tcGroup group = sequence $ map tcPredDef group
    -}


-- TypeCheck a clause
--tcClause (SClause a hd bd) = do 
    



-- TypeCheck an expression
tcExpr :: Monad m => SExpr PosSpan 
                  -> Tc m ( SExpr (Typed PosSpan) )

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
tcExpr ex@(SExpr_var a var@(Var _ _)) = do
    -- search in environment
    envTp <- asks $ lookupRho $ nameOf var
    tp <- case envTp of
        tp'@(Just _) -> return tp'
        Nothing  -> do
            -- If not found, search in exist. vars
            st <- get 
            return $ lookup (nameOf var) (exists st)
    case tp of
        -- Found var in env+state
        Just tp' -> return $ fmap (typed tp') ex
        -- Did not find, so add it to the state
        Nothing -> do
            al <- newAlpha
            let newRho = Rho_var al
            addExist (nameOf var) newRho
            return $ fmap (typed newRho) ex

-- Anonymous variable
tcExpr ex@(SExpr_var a (AnonVar vinf)) = do
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
    let argTps = map typeOf args'
    return $ SExpr_app (typed Rho_i a) fun' args'


-- Operator, functional
tcExpr (SExpr_op a c False args) = do    
    let c' = fmap (typed Rho_i) c -- TODO: UGLY, using i type as sigma
    -- typecheck args
    args' <- mapM tcExpr args
    mapM_ (\ex -> addConstraint (typeOf ex) Rho_i ex) args'
    let argTps = map typeOf args'
    return $ SExpr_op (typed Rho_i a) c' False args'

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
tcExpr (SExpr_op a c@(Const cinf cnm) True args) = do   
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
    return $ SExpr_op a' c' False args'

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
    let bindings = zip varNames argTypes |> filter (\(nm, _) -> nm /= "_")
    -- Put variables in the environment with fresh types
    -- and typeCheck body
    bd' <- withEnvVars bindings (tcExpr bd)
    let bdType = typeOf bd'
    addConstraint bdType (Rho_pi $ Pi_var phi) bd'
    let -- Type of the whole expression 
        lamType = Rho_pi $ Pi_fun argTypes (Pi_var phi)
        -- Variables with their types
        vars'   = zip vars alphas |> 
                  map ( \(var, al) -> fmap (typed (Rho_var al)) var)
    return $ SExpr_lam (typed lamType a) vars' bd'

-- Type annotated
tcExpr (SExpr_ann _ _ _) = throwError $ internalErr "annotations not impleented yet"

-- Utility function to search environment for expr.
findPoly :: Monad m => Symbol -> Int -> Tc m RhoType
findPoly cnm ar = do
    envTp <- asks $ lookupPoly (cnm,ar)
    tp <- case envTp of 
        Nothing -> do -- Type is left free
            pi <- typeWithArity ar
            return $ Rho_pi pi
        Just envTp' -> do
            pi <- freshen envTp'
            return $ Rho_pi pi
    return tp

