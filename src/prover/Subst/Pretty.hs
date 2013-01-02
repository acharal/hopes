
module Subst.Pretty where

import Pretty
import Subst (Subst)
import Lang
import CoreLang
import Data.List (elemIndex, sortBy, groupBy)
import Data.Either (rights)
import Data.Monoid (Monoid, mappend, mempty, mconcat)

instance (Pretty a, Eq a, Symbol a) => Pretty (Subst a) where
    ppr xs = vcat $ map ppr_bind xs
        where ppr_bind (v,t) = sep [ ppr v <+> text "=", ppr t ]

class Pretty a => AnswerPrintable a where
    printanswer :: a -> Doc

instance (Symbol a, Pretty a, Eq a, Show a) => AnswerPrintable (Subst a) where
    printanswer xs = vcat $ map ppr_bind xs
        where ppr_bind (v,t) = sep [ ppr v <+> text "=", printanswer t ]

instance (Symbol a, Pretty a, Eq a, Show a) =>  AnswerPrintable (Expr a) where
    printanswer a = ppr_basic' a


data BasicExprRep a = 
    BasicSet { pos  :: [[BasicExprRep a]]   -- a set of positive tuples
             , neg  :: [[BasicExprRep a]]   -- a set of negative tuples
             , vars :: [Expr a]             -- ground representation of atom
             }
    | BasicGround (Expr a)                  -- 
    deriving Show

instance Monoid (BasicExprRep a) where
    mempty  = BasicSet [] [] []
    mappend (BasicSet p1 n1 g1) (BasicSet p2 n2 g2) = 
        BasicSet (mappend p1 p2) (mappend n1 n2) (mappend g1 g2)

mkpos e = BasicSet [e] [] []
mkneg e = BasicSet [] [e] []
mkvar e = BasicSet [] [] [e]

mkgrd e = BasicGround e

mkbasic e@CTrue     = mkgrd e
mkbasic e@CFalse    = mkgrd e
mkbasic e@(Rigid _) = mkgrd e
mkbasic e@(Var _)   = mkgrd e
mkbasic e@(App _ _) = mkgrd e
mkbasic e =
    let splitAnd (And b1 b2) = splitAnd b1 ++ splitAnd b2
        splitAnd e = [e]

        splitAndOr (And b1 b2) = splitAndOr b1 ++ splitAndOr b2
        splitAndOr (Or  b1 b2) = splitAndOr b1 ++ splitAndOr b2
        splitAndOr e = [e]

        l f v (Lambda x e) = l f (x:v) e
        l f v e = f (reverse v) e

        simple' [] e       = mkvar e
        simple' vs (Not e) = mkneg (tup vs e)
        simple' vs e       = mkpos (tup vs e)

        simple = l simple' []

        tup vs e =  map snd $ sortBy (o vs) $ q $ groupBy v $ map tupElem $ splitAnd e
            where tupElem (Eq (Var x) e) = (x, Left (mkgrd e))
                  tupElem e@(App _ _) = 
                    case functor e of
                        Var x -> (x, Right $ map mkbasic (args e))
                        _ -> error "tupElem"
                  v (x, _) (y, _) = x == y
                  q xs = map q'' $ map q' xs
                  q' [(x,y)] = (x, [y])
                  q' ((x,y):xs) = (x, y:(snd (q' xs)))
                  q'' (x, [Left y])  = (x, y)
                  q'' (x, [Right y]) = (x, mkpos y)
                  q'' (x, ys) = (x, mconcat $ map mkpos $ rights ys)
                  o r (x, _) (y, _)  = compare' (elemIndex x r) (elemIndex y r)
                      where compare' (Just n1) (Just n2) = compare n1 n2
                            compare' Nothing Nothing = EQ
                            compare' Nothing _ = GT
                            compare' _ Nothing = LT
    in  mconcat $ map simple $ splitAndOr e

instance (Symbol a, Pretty a) => Pretty (BasicExprRep a) where
    ppr = pprbasic

pprbasic (BasicGround e) = ppr e 
pprbasic (BasicSet p n v) = pprset p <+> text "except" <+> pprset n
    where pprset xs = curly $ sep $ punctuate comma $ map pprtuple xs
          pprtuple [x] = ppr x
          pprtuple xs  = parens $ sep $ punctuate comma $ map ppr xs

ppr_basic' e = pprbasic (mkbasic e)

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
       f' [] [e] = Left [ppr e]
       f' vs es  = Right $ map snd $ sortBy (\(x,_)-> \(y,_) -> orderref vs x y) $ q $ map ppr_simple es

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

   in  ppr_set $ map ppr_tuple $ rights $ map (l f' []) $ splitOr e

instance (Pretty a, Symbol a) => Pretty (Expr a) where
    ppr (CTrue)      = text "true"
    ppr (CFalse)     = text "false"
    ppr (Rigid p)    = ppr p
    ppr (Var v)      = ppr v
    ppr e@(App _ _)  = ppr (functor e) <> parens (sep $ punctuate comma (map ppr (args e)))
    ppr (And e1 e2)  = sep $ punctuate comma [ppr e1, ppr e2]
    ppr (Or e1 e2)   = sep $ punctuate (text ";") [ppr e1, ppr e2]
    ppr (Eq e1 e2)   = sep $ [ppr e1, text "=" , ppr e2]
    ppr (Lambda x e) = parens (text "\\" <> ppr x <> text "^" <> ppr e)
    ppr (Not e1)     = text "not" <> parens (ppr e1)
    ppr (Exists v e) = parens (text "E" <> ppr v <+> ppr e)
    ppr (Forall v e) = parens (text "F" <> ppr v <+> ppr e)
