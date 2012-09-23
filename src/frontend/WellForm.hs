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

-- | checking well formatted formulas
module WellForm (wfp, wfg) where

import Language.Hopl.Syntax
import Types (MonoTypeV(..), TyEnv, tyAll, hasType, typeOf)
import Loc (Located(..))
import Tc (Tc, getTypeEnv, normType)
import TypeCheck (tcProg, tcForm)
import Restrict (restrictProg, restrictForm)


-- well formatted program
wfp p = do
    (p', env)  <- tcProg p
    (p'', env) <- restrictProg (p', env)
    env <- zonkEnv env
    return (p', env)

-- well formatted formula
wff f = do
    f' <- tcForm f
    restrictForm f'
    return f'

-- well formatted goal.
-- this is different from formula
-- because it may contain *new* symbols
wfg g = do
    g'  <- wff g
    env <- getTypeEnv
    env <- zonkEnv env
    return (g', env)

-- head must not contain "other than" higher order variables in higher-order arg positions
zonkEnv :: (TyEnv a) -> Tc (TyEnv a)
zonkEnv env =
    let aux (v,t) = do
            t' <- zonkType t
            return (v, t')
    in  mapM aux env


zonkType (TyVar _) = return tyAll
zonkType (TyFun t1 t2) = do
    t1' <- zonkType t1
    t2' <- zonkType t2
    return (TyFun t1' t2')
-- zonkType (TyTup tl) = do
--    tl' <- mapM zonkType tl
--    return (TyTup tl')
zonkType t = return t

-- normProg :: HpProg a -> Tc (HpProg a)
normSrc p = do
    cl <- mapM normForm (clauses p)
    return (p { clauses = cl })

normForm (L l (HpClause b xs ys)) = do
    b' <- normBinds b
    xs' <- mapM normExpr xs
    ys' <- mapM normExpr ys
    return (L l (HpClause b' xs' ys'))

normBinds bs = mapM normBind bs
    where normBind (HpBind s ty) = do
            ty' <- normType ty
            s'   <- normSym s
            return (HpBind s' ty')

normExpr (L l (HpApp e es)) = do
    e'  <-  normExpr e
    es' <- mapM normExpr es
    return (L l (HpApp e' es'))
normExpr (L l (HpTup es)) = do
    es' <- mapM normExpr es
    return (L l (HpTup es'))
normExpr (L l (HpSym s)) = do
    s' <- normSym s
    return (L l (HpSym s'))

normSym s = do
    ty' <- normType (typeOf s)
    return $ hasType ty' s
