module CoreLang where

import qualified Language.Hopl as H

import Lang (ceq, cand, cor, ctop, cbot)
import Data.List (union, (\\))
import Types
import Data.Monoid

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
--    | Forall a (Expr a)
    | Var  a			-- variable
    | Rigid a			-- predicate and function symbol
    | Cut
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
-- fv (Forall a e) = filter (/= a) $ fv e


splitExist (Exists v e1) = ((v:vs), e')
    where (vs, e') = splitExist e1
splitExist e = ([], e)

exists vs e = foldr Exists e vs

hoplToCoreExpr e@(H.App (H.App op e1) e2) 
    | op == ceq  = Eq  c1 c2
    | op == cand = And c1 c2
    | op == cor  = Or  c2 c2
    | otherwise  = hoplToCoreExpr' e
   where c1 = hoplToCoreExpr e1
         c2 = hoplToCoreExpr e2
hoplToCoreExpr e = hoplToCoreExpr' e

hoplToCoreExpr' (H.App e1 e2)  | H.isNot e1  = Not (hoplToCoreExpr e2)
                               | otherwise   = App (hoplToCoreExpr e1) (hoplToCoreExpr e2)
hoplToCoreExpr' (H.Lambda a e) = Lambda a (hoplToCoreExpr e)
hoplToCoreExpr' (H.Flex a)     = Var a
hoplToCoreExpr' c@(H.Rigid a)  | c == ctop = CTrue
                               | c == cbot = CFalse
                               | H.isCut c = Cut
                               | otherwise = Rigid a

hoplToCoreClause c@(H.C a e) = (a, closed e)
   where  closed' (Lambda x e) vs = Lambda x (closed' e (x:vs)) 
          closed' e vs = foldr exists e (fv e \\ vs)
          closed e   = closed' (hoplToCoreExpr e) []
          exists v e = Exists v e

hopltoCoreGoal e = hoplToCoreExpr e

kbtoProgram kb = Prog (map hoplToCoreClause (H.clauses kb))

