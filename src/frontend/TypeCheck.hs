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

{-# LANGUAGE NoMonomorphismRestriction #-}

-- | check for well typed expressions and formulas
module TypeCheck (tcProg, tcExpr, tiExpr, tcForm) where

import Language.Hopl.Syntax
import Lang (Sym(AnonSym), liftSym)
import Types (tyBool, tyAll, MonoTypeV(..))
import Tc

import Loc (Located(..))
import Pretty

import Control.Monad (mapAndUnzipM, when)
import Data.Foldable (foldlM)
import Data.Monoid (mappend)

{-
    Strategy of type checking

    Alternative I.

    1. tc functions just type checks the source without annotate the syntax tree with types.
    2. tc functions just collect Constraints and fail if cannot satisfy constraints.
    3. The main problem is that local bindings does not exist in the global type environment.
       As a result after the type check of a quantified formula (or lambda expression) we have to
       store somewhere the types (or tyvars) of those bindings.

    Alternative II.

    1. tc functions check and if succeed annotate the source with types.
    2. syntax tree is already parametrized with the symbol type, so the new syntax tree must have
       a new symbol type that can hold other info about the symbol such as type and arity.
    3. The annotation of the tree cannot be performed before the type checking completes. Types of
       symbols (and therefore local bindings) are not fixed during the type checking.
       As a result, the real annotation must happen *after* the type checking.

    Ugly thinks
    1. Location information (helps on error messagings but it is not part of the logic of type checking)
    2. Context information (as in 1)
    3. Bindings and annotation (annotation helps next stages but it's checking not annotating)

-}

buildinsigs = map (\(x,t) -> (liftSym x, t) ) buildins'

buildins' =
    [ (".",     TyFun tyAll (TyFun tyAll tyAll))
    , ("[]",    tyAll)
    , ("s",     TyFun tyAll tyAll)
    , ("true",  tyBool)
    , ("false", tyBool)
    , ("0",     tyAll)
    , ("=",     TyFun tyAll  (TyFun tyAll tyBool))
    , (",",     TyFun tyBool (TyFun tyBool tyBool))
    , (";",     TyFun tyBool (TyFun tyBool tyBool))
    , ("_",     tyAll)
    ]

-- type checking and inference

tcProg src = do
    withSig src $ do
        withTypeEnv (tyEnv src `mappend` buildinsigs) $ do
            cls' <- mapM tcForm $ clauses src
            ty_env  <- normEnv
            return (src{ clauses = cls' }, ty_env)

-- tcForm :: LHpFormula a -> Tc (LHpFormula b)
tcForm f@(L l (HpClause b xs ys)) =
    enterContext (CtxtForm f) $ do
    tvs <- newTyBindings (bindings f)
    extendEnv tvs $ do
        xs' <- mapM tcAtom xs
        ys' <- mapM tcAtom ys
        b' <- tyBinds b
        return (L l (HpClause b' xs' ys'))

tyBinds = mapM tyBind
    where tyBind (HpBind s oldty) = do
            ty <- lookupVar s
            ty <- normType ty
            s' <- tySym s
            return (HpBind s' ty)

newTyBindings b = mapM (freshTyFor.symbolBind) b

-- tcAtom, tcAtom' :: LHpAtom a -> Tc ()
tcAtom a = enterContext (CtxtAtom a) $ tcExpr tyBool a

-- tiExpr :: LHpExpr a -> Tc (MonoType, LHpExpr)
tiExpr e = do
    var_ty <- freshTyVar
    e' <- tcExpr var_ty e
    return (var_ty, e')

-- tcExpr, tcExpr' :: LHpExpr a -> MonoType -> Tc ()

tcExpr  exp_ty e = enterContext (CtxtExpr e) $ tcExpr' exp_ty e

tcExpr' exp_ty (L _ (HpPar  e)) = tcExpr exp_ty e

tcExpr' exp_ty (L _ (HpAnn e ty)) = do
    ann_ty <- instantiate ty
    e' <- tcExpr ann_ty e
    unify ann_ty exp_ty
    return e'

tcExpr' exp_ty (L l (HpApp e args))  = do
    (fun_ty, e')     <- tiExpr e
    (args_ty, args') <- mapAndUnzipM tiExpr args
    -- let tup_ty = case args_ty of
    --                 [x] -> x
    --                 tys -> TyTup tys
    let arg'_ty = head args_ty
    let res'_ty =  foldr (\a -> \t -> TyFun a t) exp_ty (tail args_ty)
    (arg_ty, res_ty) <- unifyFun fun_ty
    unify arg_ty arg'_ty
    unify res_ty res'_ty
    return (L l (HpApp e' args'))

tcExpr' _ (L l (HpSym AnonSym)) = return (L l (HpSym AnonSym))

tcExpr' exp_ty (L l (HpSym s))  = do
    sym_ty <- lookupVar s
    unify sym_ty exp_ty
    s' <- tySym s
    return (L l (HpSym s'))

tcExpr' exp_ty (L l (HpLam bs e)) = do
    tvs <- newTyBindings bs
    extendEnv tvs $ do
        (body_ty, e')  <- tiExpr e
        let fun_ty = foldl (\a -> \(_,t) -> TyFun t a) body_ty (reverse tvs)
        unify fun_ty exp_ty
        bs' <- tyBinds bs
        return (L l (HpLam bs' e'))

--tcExpr' exp_ty (L l (HpTup es)) = do
--    (tys, es') <- mapAndUnzipM tiExpr es
--    unify (TyTup tys) exp_ty
--    return (L l (HpTup es'))

-- unification

-- unify :: Monad m => MonoType -> MonoType -> Tc m ()
unify (TyVar v1) t@(TyVar v2)
    | v1 == v2    = return ()
    | otherwise   = unifyVar v1 t

unify (TyVar v) t = unifyVar v t
unify t (TyVar v) = unifyVar v t

unify (TyFun fun1 arg1) (TyFun fun2 arg2) = do
    unify fun1 fun2
    unify arg1 arg2

-- unify t@(TyTup tys) t'@(TyTup tys')
--    | length tys == length tys' = zipWithM_ unify tys tys'
--    | otherwise                 = unificationErr t t'

unify t t'
    | t == t'     = return ()
    | otherwise   = unificationErr t t'


unifyFun t = do
    arg_ty  <- freshTyVar
    res_ty  <- freshTyVar
    unify (TyFun arg_ty res_ty) t
    return (arg_ty, res_ty)

unifyVar v t = do
    maybe_ty <- lookupTy v
    case maybe_ty of
        Nothing -> varBind v t
        Just t' -> unify t' t

varBind  v1 t@(TyVar v2) = do
    maybe_ty <- lookupTy v2
    case maybe_ty of
        Nothing -> addConstraint v1 t
        Just ty -> unify (TyVar v1) ty

varBind v ty = do
    tvs <- tyvarsM ty
    when (v `elem` tvs) (occurCheckErr v ty)
    addConstraint v ty


-- utilities

tyvarsM = foldlM (\l -> \v -> auxM v >>= \l' -> return (l' ++ l)) []
    where auxM v = lookupTy v >>= \mayv' ->
            case mayv' of
                Nothing -> return []
                Just ty -> do
                    vs <- tyvarsM ty
                    return (v:vs)


-- tySym :: HpSymbol -> Tc HpSymbol
tySym = return . id   --no annotation

-- error reporting

unificationErr inf_ty exp_ty = do
        inf_ty' <- normType inf_ty
        exp_ty' <- normType exp_ty
        let desc = hang (text "Could not match types:") 4
                        (vcat [ text "Inferred:" <+> ppr inf_ty',
                                text "Expected:" <+> ppr exp_ty' ])
        typeError desc


occurCheckErr tv ty = typeError (text "Could not construct an infinite type")
