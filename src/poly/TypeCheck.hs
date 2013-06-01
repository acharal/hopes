module TypeCheck where

import Basic
import Prepr
import TcUtils
import Syntax
import Types
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


tcExpr :: Monad m => SExpr a 
                  -> Tc m ( SExpr (Typed a) )

-- Individual constant
tcExpr ex@(SExpr_const _ _ False _ _) =
    return $ fmap (typed Rho_i) ex 
    -- fmap conveniently maps content to a typed content


-- Predicate constant, first case
tcExpr ex@(SExpr_const _ c True _ _) = do
    tp <- findPoly (nameOf c) ex
    return $ fmap (typed tp) ex

-- Predicate constant, second case
tcExpr ex@(SExpr_predCon _ c _ _) = do 
    tp <- findPoly (nameOf c) ex 
    return $ fmap (typed tp) ex


-- Variable
tcExpr ex@(SExpr_var a var) = do
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



-- Predicate operator
tcExpr ex@(SExpr_op _ c True _) = do
    tp <- findPoly (nameOf c) ex
    return $ fmap (typed tp) ex





-- Number
tcExpr (SExpr_number a num) = 
    return $ SExpr_number (typed Rho_i a) num

tcExpr _ = error "not implemented yet"

-- Utility function to search environment for expr.
findPoly cnm ex = do
    let ar = fromJust $ arity ex
    envTp <- asks $ lookupPoly (cnm,ar)
    tp <- case envTp of 
        Nothing -> do -- Type is left free
            pi <- typeWithArity ar
            return $ Rho_pi pi
        Just envTp' -> do
            pi <- freshen envTp'
            return $ Rho_pi pi
    return tp

