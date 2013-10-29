
module HoplToCore where

import CoreLang
import Lang (ceq, cand, cor, ctop, cbot)
import qualified Language.Hopl as H
import Data.List ((\\))

hoplToCoreExpr e@(H.App (H.App op e1) e2) 
    | op == ceq  = Eq  c1 c2
    | op == cand = And c1 c2
    | op == cor  = Or  c2 c2
    | op == H.cons = ListCons c1 c2
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
                               | c == H.nil  = ListNil
                               | otherwise = Rigid a

hoplToCoreClause c@(H.C a e) = (a, closed e)
   where  closed' (Lambda x e) vs = Lambda x (closed' e (x:vs)) 
          closed' e vs = foldr exists e (fv e \\ vs)
          closed e   = closed' (hoplToCoreExpr e) []
          exists v e = Exists v e

hopltoCoreGoal e = hoplToCoreExpr e

kbtoProgram kb = Prog (map hoplToCoreClause (H.clauses kb))
