
module Subst.Pretty where

import Pretty
import Subst
-- import Language.Hopl
-- import Language.Hopl.Pretty ()
import Lang
import CoreLang
import Data.List (elemIndex, sortBy, groupBy)
import Data.Either (rights)
import Debug.Trace

instance (Pretty a, Eq a, Symbol a) => Pretty (Subst a) where
    ppr xs = vcat $ map ppr_bind xs
        where ppr_bind (v,t) = sep [ ppr v <+> text "=", ppr t ]

class Pretty a => AnswerPrintable a where
    printanswer :: a -> Doc

instance (Symbol a, Pretty a, Eq a, Show a) => AnswerPrintable (Subst a) where
    printanswer xs = vcat $ map ppr_bind xs
        where ppr_bind (v,t) = sep [ ppr v <+> text "=", printanswer t ]

instance (Symbol a, Pretty a, Eq a, Show a) =>  AnswerPrintable (Expr a) where
    printanswer a = ppr_basic a

ppr_basic e@CTrue     = ppr e
ppr_basic e@CFalse    = ppr e
ppr_basic e@(Rigid _) = ppr e
ppr_basic e@(Var _)   = ppr e
ppr_basic e@(App _ _) = ppr e
ppr_basic e = 
   let splitOr (Or b1 b2) = splitOr b1 ++ splitOr b2
       splitOr e = [e]
       splitAnd (And b1 b2) = splitAnd b1 ++ splitAnd b2
       splitAnd e = [e]
       -- l accums bound variables from lambda abstraction and split inner expression using "and"
       -- this are provided to "f" in order to produce a list of printable Doc.
       l f v (Lambda x e) = l f (x:v) e
       l f v e = f (reverse v) (splitAnd e)  -- f :: [a] -> [Expr a] -> [Doc]
       orderref ref x y = compare' (elemIndex x ref) (elemIndex y ref)
           where compare' (Just n1) (Just n2) = compare n1 n2
                 compare' Nothing Nothing = EQ
                 compare' Nothing _       = GT
                 compare' _ Nothing       = LT
       f' [] [e] = [ppr e]
       f' vs es = map snd $ sortBy (\(x,_)-> \(y,_) -> orderref vs x y) $ q $ map ppr_simple es
       q es = map q'' $ map q' $ groupBy (\(x,_) -> \(y,_) -> x == y) es
       q'' (x, [Left y])  = (x, y)
       q'' (x, [Right y]) = (x, ppr_set [y])
       q'' (x, ys) = (x, ppr_set (rights ys))
       q' [(x,y)] = (x, [y])
       q' ((x,y):xs) = (x, (y:snd (q' xs)))
       ppr_simple (Eq (Var x) e) = (x, Left (ppr e))
       ppr_simple e@(App _ _) = 
            case functor e of
                Var f -> (f, Right (ppr_tuple (map ppr_basic (args e))))
       ppr_tuple [x] = x
       ppr_tuple xs  = parens $ sep $ punctuate comma xs
       ppr_set xs = curly $ sep $ punctuate comma xs
   in  ppr_set $ map (\e -> ppr_tuple (l f' [] e)) $ splitOr e

instance (Pretty a, Symbol a) => Pretty (Expr a) where
    ppr (CTrue)     = text "true"
    ppr (CFalse)    = text "false"
    ppr (Rigid p)   = ppr p
    ppr (Var v)     = ppr v
    ppr e@(App _ _) = ppr (functor e) <> parens (sep $ punctuate comma (map ppr (args e)))
