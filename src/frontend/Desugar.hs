--  Copyright (C) 2006-2008 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

{-# LANGUAGE
    FlexibleContexts
   ,NoMonomorphismRestriction
#-}

-- | Transform Syntax to plain Hopl removing superfluous information (e.g. Location)
module Desugar where

import Language.Hopl hiding (clauses)
-- import qualified KnowledgeBase as KB
import Language.Hopl.Syntax hiding(bindings)
import Lang (cand, ceq, ctop, Sym(AnonSym), liftSym)
import Types (TyEnv, Typed, tyAll, typed, order, typeOf, MonoTypeV(..))
import Loc (unLoc, Located(..))
import Control.Monad.Reader (ReaderT, runReaderT, local, asks)
import Control.Monad.State (StateT, evalStateT, get, modify)


data DesugarEnv = DSEnv { rigty :: TyEnv HpSymbol, bindings :: [HpBindings HpSymbol] }

type DesugarT m = ReaderT DesugarEnv (StateT Int m)

runDesugarT m = evalStateT (runReaderT m (DSEnv [] [])) 0

-- desugarSrc :: Monad m => (HpSrc HpSymbol, TyEnv HpSymbol) -> DesugarT m (KB.KnowledgeBase (Typed HpSymbol))
desugarSrc :: Monad m => (HpSrc HpSymbol, TyEnv HpSymbol) -> DesugarT m [Clause (Typed HpSymbol)]
desugarSrc (p, ty_env) = local (\r -> r{ rigty = ty_env}) $ do
        cl <- mapM (desugarClause.unLoc) (clauses p)
        return cl

desugarGoal ((L _ (HpClause b [] ys)),ty_env) =
    local (\r -> r{ rigty = ty_env, bindings = b:(bindings r)}) $ do
    bd <- mapM desugarExp (map unLoc ys)
    desugarTupApp cand bd

desugarClause (HpClause b [] ys)  = fail "Cannot transform clause without a head"
desugarClause (HpClause b [x] ys) =
    local (\r -> r {bindings = b:(bindings r)}) $ do
    -- h <- desugarExp (unLoc x)
    b <- mapM desugarExp (map unLoc ys)
    (Rigid p) <- desugarExp (unLoc (funcOf x))
    b2 <- desugarTupApp cand b
    body <- bindAndUnify (map unLoc (argsOf x)) b2
    return (C p body)


bindAndUnify ls b =
    let bindAndUnify' body [] vs es      = return $ foldr Lambda body' (reverse vs)
                where appAnd e e' = App (App cand e) e' 
                      body' = if body == ctop && not(null es)
                              then foldr1 appAnd (reverse es)
                              else foldr appAnd body (reverse es)
        bindAndUnify' body as@((Flex vv):xs) vs es | vv `elem` vs = bindAndUnify'' body as vs es
                                                   | otherwise    = bindAndUnify' body xs (vv:vs) es
        bindAndUnify' body as vs es = bindAndUnify'' body as vs es

        bindAndUnify'' body (a:as) vs es =
             if order a == 0 then do
                 v' <- freshVar >>= \x -> return (typed (typeOf a) x)
                 bindAndUnify' body as (v':vs) ((App (App ceq (Flex v')) a):es) 
             else 
                 fail ("Higher order term, not variable" ++ (show a) )
    in do
        as <- mapM desugarExp ls
        bindAndUnify' b as [] []

desugarAppTup a ls =
    return $ foldl (\e -> \b -> App e b) a ls

desugarTupApp _ [] =  return ctop
desugarTupApp _ [x] = return x
desugarTupApp a (l:ls) = do
    r <- desugarTupApp a ls
    return $ App (App a l) r

desugarExp (HpApp e es') = do
    ce   <- desugarExp (unLoc e)
    ces' <- mapM desugarExp (map unLoc es')
    desugarAppTup ce ces'

{-
desugarExp (HpTup es) = do
    ces <- mapM desugarExp (map unLoc es)
    return (Tup ces)
-}

desugarExp (HpPar e)  = desugarExp (unLoc e)

desugarExp (HpSym AnonSym) = return $ Flex (typed tyAll AnonSym)

desugarExp (HpSym a)  =
    let grd (TyFun t1 t2) = TyFun (grd t1) (grd t2)
        grd (TyGrd c) = TyGrd c
        grd (TyVar _) = tyAll
        --grd (TyTup ts) = TyTup (map grd ts)
    in do
    te <- asks rigty
    le <- asks bindings
    case lookup a te of
        Nothing -> do
            case le of
                []    -> error ("wtf? no definition for variable "++ show a)
                (l:_) ->
                    case lookupBind a l of
                        Nothing -> error "not implemented yet"
                        Just (HpBind v' ty) -> return $ Flex (typed (grd ty) v')
        Just ty ->
             case ty of
                TyVar _ -> error (show a)
                _ -> return $ Rigid (typed ty a)

desugarExp (HpLam bs e) =
    local (\r -> r {bindings = bs:(bindings r)}) $ do
    e' <- desugarExp (unLoc e)
    let f (HpBind x ty) = typed ty x
    return $ foldl (\a -> \b -> (Lambda (f b) a)) e' bs

desugarExp e = error "Expression must not occur in that phase"

-- freshVar :: (MonadState Int m, Monad m, Symbol a) => m a
freshVar = do
    r <- get
    modify (+1)
    return $ liftSym $ "_" ++ (show r)

