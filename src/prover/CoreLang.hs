module CoreLang where

import Types (HasType(..), hasType, tyBool, MonoTypeV(TyFun))
import Data.Monoid
import Data.List (union)

data Expr a = 
      CTrue
    | CFalse
    | And (Expr a) (Expr a)	-- Expr :/\: Expr
    | Or  (Expr a) (Expr a)	-- Expr :\/: Expr
    | Lambda a (Expr a)
    | App (Expr a) (Expr a)
    | Eq  (Expr a) (Expr a)	-- Expr :=: Expr 
    | Not (Expr a)		    -- :~: Expr
    | Exists a (Expr a)
    | Var  a			-- variable
    | Rigid a			-- predicate and function symbol
    | Cut
    | ListCons (Expr a) (Expr a)
    | ListNil
   deriving (Eq, Show)



instance HasType a => HasType (Expr a) where
    typeOf (And _ _) = tyBool
    typeOf (Or  _ _) = tyBool
    typeOf (Eq  _ _) = tyBool
    typeOf (CTrue)   = tyBool
    typeOf (CFalse)  = tyBool
    typeOf (Exists _ _) = tyBool
--    typeOf (Forall _ _) = tyBool
    typeOf (Rigid p) = typeOf p
    typeOf (Var   v) = typeOf v
    typeOf (App e e') = case typeOf e of 
                            TyFun _ ty_res -> ty_res
                            t -> error ("not expected type " ++ show t)
    typeOf (Lambda v e) = TyFun (typeOf v) (typeOf e)
    hasType ty e = error "hasType"

data Program a = Prog { clauses :: [(a,Expr a)] }

instance Monoid (Program a) where
    mempty = Prog mempty
    mappend (Prog p1) (Prog p2) = Prog (p1 `mappend` p2)

clausesOf r p = map snd $ filter (\(q,_) -> q == r) (clauses p)

functor (App e a) = functor e
functor e = e

args e = args' e []
  where args' (App e a) as = args' e (a:as)
        args' _ as = as

fv (Var a)      = [a]
fv (Rigid _)    = []
fv (CTrue)      = []
fv (CFalse)     = []
fv (Cut)        = []
fv (Not e)      = fv e
fv (App e1 e2)  = fv e1 `union` fv e2
fv (And e1 e2)  = fv e1 `union` fv e2
fv (Or  e1 e2)  = fv e1 `union` fv e2
fv (Eq  e1 e2)  = fv e1 `union` fv e2
fv (Lambda a e) = filter (/= a) $ fv e
fv (Exists a e) = filter (/= a) $ fv e
fv (ListCons e1 e2) = fv e1 `union` fv e2
fv (ListNil) = []
-- fv (Forall a e) = filter (/= a) $ fv e


splitExist (Exists v e1) = ((v:vs), e')
    where (vs, e') = splitExist e1
splitExist e = ([], e)

splitLambda (Lambda v e1) = ((v:vs), e')
    where (vs, e') = splitLambda e1
splitLambda e = ([], e)

exists vs e = foldr Exists e vs
lambda vs e = foldr Lambda e vs


isVar (Var _) = True
isVar _ = False

