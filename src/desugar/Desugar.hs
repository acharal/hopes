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

-- | Transform Syntax to plain Hopl removing superfluous information (e.g. Location)
module Desugar where

import Hopl
-- import qualified KnowledgeBase as KB
import Syntax hiding(bindings)
import Symbol
import Types
import Loc
import Control.Monad.Reader

data DesugarEnv = DSEnv { rigty :: TyEnv HpSymbol, bindings :: [HpBindings HpSymbol] }

type DesugarT = ReaderT DesugarEnv

runDesugarT m = runReaderT m (DSEnv [] [])

-- desugarSrc :: Monad m => (HpSrc HpSymbol, TyEnv HpSymbol) -> DesugarT m (KB.KnowledgeBase (Typed HpSymbol))
desugarSrc :: Monad m => (HpSrc HpSymbol, TyEnv HpSymbol) -> DesugarT m [Clause (Typed HpSymbol)]
desugarSrc (p, ty_env) = local (\r -> r{ rigty = ty_env}) $ do
        cl <- mapM (desugarClause.unLoc) (clauses p)
        --return $ KB.KB { KB.clauses = cl }
        return cl

desugarGoal ((L _ (HpClause b [] ys)),ty_env) =
    local (\r -> r{ rigty = ty_env, bindings = b:(bindings r)}) $ do
    bd <- mapM desugarExp (map unLoc ys)
    return bd

desugarClause (HpClause b [] ys)  = fail "Cannot transform clause without a head"
desugarClause (HpClause b [x] ys) =
    local (\r -> r {bindings = b:(bindings r)}) $ do
    h <- desugarExp (unLoc x)
    b <- mapM desugarExp (map unLoc ys)
    return (h, b)

desugarExp (HpApp e es') = do
    ce   <- desugarExp (unLoc e)
    ces' <- mapM desugarExp (map unLoc es')
    ce'  <-
        case ces' of
            [x] -> return x
            xs -> return (Tup xs)
    return (App ce ce')

desugarExp (HpTup es) = do
    ces <- mapM desugarExp (map unLoc es)
    return (Tup ces)

desugarExp (HpPar e)  = desugarExp (unLoc e)

desugarExp (HpSym AnonSym) = return $ Flex (typed tyAll AnonSym)

desugarExp (HpSym a)  =
    let grd (TyFun t1 t2) = TyFun (grd t1) (grd t2)
        grd (TyGrd c) = TyGrd c
        grd (TyVar _) = tyAll
        grd (TyTup ts) = TyTup (map grd ts)
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

desugarExp e = error "Expression must not occur in that phase"
