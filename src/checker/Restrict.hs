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

module Restrict where

import Syntax
import Types
import Tc
import TypeCheck (tiExpr, tcExpr)
import Error (catchError)
import Loc
import Pretty
import Monad (when)
import List (partition)
import Maybe (catMaybes)

{-
    Well-formated formulas

    1. Well - typed
    2. Definitional

    I. Restrictions on the head atom A

    Let head A of form A = R( t_1, ..., t_n )

    1. R not a bounded variable
    2. t_1 | ... | t_n does not contain any ho expression that is not a bounded variable.
    3. for every t_i and t_j, j != i
       if t_i and t_j are ho bounded variables then t_i != t_j.
       In other words bounded variables that occur in the head *must* be distinct.
    4. if t_i is an application e(t'_1, ... t'_k) then must be of type i and every
       t'_i must be of type i. Applications (or partial applications) of relations e
       must not occur in head.
        4.1 e must not be bounded variable.

    II. Restrictions on the body {L_i}

    Let L_i an atom in B of form L(t_1, ..., t_n)

    5. if t_i is an application f(t'_1, ..., t'_k) must be of type i. Moreover,
       t'_i must be of type i. No (partial) application of relations allowed.
       a function symbol must be of type (i, ..., i) -> i. No ho expressions allowed
       as arguments of function symbols.

    III. Zonking Environment

    1. No polymorfism allowed. If any type variable exist must be set to type i.

    IV. Higher Order Expressions

    1. After the type checking, some expressions are polymorphic, meaning that these
       expression can be either higher order or zero order.

    V. Strategy

    1. Restrict as much as you can the types. Apply first checks that function
       symbols must be of type i.
    2. Do the 3 check. Find all variables that occur more than once in the head and
       *restrict* them to be of type i (because they can't be higher order)
    4. Check any higher order expressions occurs other than bounded variables.
    5. Check no1 can be done whenever.

    VI. Utilities

    1. arguments of an application  - done
    2. check if an expression is an application - easy
    3. match expressions with types

-}

-- wffcSource :: (HpSource a, TypeEnv) -> Tc (HpSource a, TypeEnv)

restrictProg (p, tyenv) = do
    extendEnv tyenv $ do
        mapM_ restrictForm (clauses p)
        tyenv <- normEnv
        return (p, tyenv)

restrictForm f@(L _ (HpClause _ xs ys)) = enterContext (CtxtForm f) $ do
    let locals = map (\(HpBind a t) -> (a, t)) (bindings f)
    extendEnv locals $ do
        mapM_ restrictFunc xs
        --mapM_ restrictFunc ys
        restrictHead f

-- function symbol application must be (i, ..., i) -> i
-- restrictFunc
restrictHead   (L _ (HpClause _ [] _))  = return ()
restrictHead f@(L _ (HpClause b [h] _)) = do
    let func    = funcOf h
        args    = argsOf h
        (var_args, rest_args) = partition (expBind f) args
        expBind f (L _ (HpSym s)) = isBinding f s
        expBind _ _ = False
        unLocEq x y = (unLoc x) == (unLoc y)
        varoccu = occurences unLocEq var_args

    when (expBind f func) $
        varInHead func

    let restrToAll x = (tcExpr tyAll x >> return Nothing) `catchError` \e -> return (Just x)
    maybe_errors <- mapM (\(x, _) -> restrToAll x) $ filter ((>1) . length . snd) varoccu
    let notRestricted = catMaybes $ maybe_errors
    when (not $ null $ notRestricted) (multiHoOccurErr notRestricted)

    maybe_preds <- mapM restrToAll $ filter isSymbol rest_args
    let preds = catMaybes $ maybe_preds
    when (not $ null $ preds) (predInHeadErr preds)
    --liftIO $ print (map (ppr.unLoc) (filter isSymbol rest_args))
    return ()

restrictFunc e = do
--    let args = argsOf e
--        apps = filter isApp args    --arguments that are applications
    -- mapM  (tcExpr tyAll) apps
    (ty,_) <- tiExpr e
    when (ty == tyAll) $
        mapM_ restrictInsideFunc (filter isApp (argsOf e))
restrictInsideFunc e = do
    let args = argsOf e
    mapM_ (tcExpr tyAll) args
    mapM_ restrictInsideFunc args

occurences eq [] = []
occurences eq (x:xs) =
    let (s, r) = partition (eq x) (x:xs)
    in  (x, s):(occurences eq r)

multiHoOccurErr occlist =
    typeError (sep ([text "Higher order bound variables:",
                     nest 4 (sep (punctuate comma (map ppr_aux occlist))),
                     text "must occur only once as argument in the head of a clause"]))
    where ppr_aux e = quotes $ ppr $ unLoc e

predInHeadErr preds =
    typeError (sep ([text "Predicate variables:",
                     nest 4 (sep (punctuate comma (map ppr_aux preds))),
                     text "must not occur as argument in the head of a clause"]))
    where ppr_aux e = quotes $ ppr $ unLoc e


varInHead var =
    typeError (sep ([text "Bounded variable:",
                     nest 4 (quotes (ppr (unLoc var))),
                     text "must not occur as functor in the head of a clause"]))

