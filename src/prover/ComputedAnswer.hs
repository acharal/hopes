{-# LANGUAGE
    FlexibleInstances
   ,TypeSynonymInstances
#-}

module ComputedAnswer (ComputedAnswer(..)) where


import Pretty
import Subst (Subst)
import Lang
import CoreLang
import Data.List (elemIndex, sortBy, groupBy)
import Data.Either (rights)
import Data.Monoid (Monoid, mappend, mempty, mconcat)

data ComputedAnswer a = Computed (Subst a) [Expr a]


instance (Pretty a, Symbol a, Show a) => Pretty (Subst a) where
    ppr xs = vcat $ map ppr_bind xs
        where ppr_bind (v,t) = sep [ ppr v <+> text "=", ppr t ]

instance (Pretty a, Eq a, Symbol a, Show a) => Pretty (ComputedAnswer a) where
    ppr (Computed s e) = vcat [ pprsubst s, ppr_expr' e ]
        where pprsubst  xs  = vcat $ map (ppr_bind "=" []) xs
              ppr_bind  s vs (v,t) = sep [ ppr v <+> text s, ppr_expr vs t ]
              ppr_expr' ies = vcat $ map aux ies
              aux (Not e) = 
                let (v', Eq t1 t2) = splitExist e
                in ppr_bind "/=" v' (t1, t2)

ppr_expr vs (Var v) | v `elem` vs = text "*" <> ppr v
                    | otherwise   = ppr v
ppr_expr vs (Rigid x) = ppr x
ppr_expr vs e@(App _ _) = ppr (functor e) <> parens (sep $ punctuate comma (map (ppr_expr vs) (args e)))
ppr_expr vs e@(Exists _ _) = 
    let (vs', e') = splitExist e
    in ppr_expr (vs ++ vs') e'
ppr_expr vs e = ppr_basic' e


data BasicTuple a = BasicTuple [a] [BasicExprRep a]
    deriving Show

data BasicExprRep a = 
    BasicSet { pos  :: [BasicTuple a]   -- a set of positive tuples
             , neg  :: [BasicTuple a]   -- a set of negative tuples
             , vars :: [Expr a]         -- ground representation of atom
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

        tup vs e = BasicTuple v' es
            where (v', e') = splitExist e
                  es = map snd $ sortBy (o vs) $ q $ groupBy v $ map tupElem $ splitAnd e'
--                  tupElem :: Expr a -> (a, Either (BasicExprRep a) (BasicTuple a))
                  tupElem (Eq (Var x) e) = (x, Left (mkgrd e))
                  tupElem e@(App _ _) = 
                    case functor e of
                        Var x -> (x, Right $ BasicTuple [] (map mkbasic (args e)))
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

instance (Symbol a, Eq a, Pretty a, Show a) => Pretty (BasicExprRep a) where
    ppr e = ppr' [] e
        where ppr' vs (BasicGround e)  = ppr_expr vs e
              ppr' vs (BasicSet p n v) = (pprset vs p) <+> text "except" <+> (pprset vs n)
              pprset vs xs = curly $ sep $ punctuate comma $ map (pprtuple vs) xs
              pprtuple vs' (BasicTuple vs [x]) = ppr' (vs' ++ vs) x
              pprtuple vs' (BasicTuple vs xs)  = parens $ sep $ punctuate comma $ map (ppr' (vs' ++ vs)) xs

ppr_basic' e = ppr (mkbasic e)

instance (Pretty a, Symbol a, Show a) => Pretty (Expr a) where
    ppr (CTrue)      = text "true"
    ppr (CFalse)     = text "false"
    ppr (Rigid p)    = ppr p
    ppr (Var v)      = ppr v
    ppr e@(App _ _)  = ppr (functor e) <> parens (sep $ punctuate comma (map ppr (args e)))
    ppr (And e1 e2)  = sep $ punctuate comma [ppr e1, ppr e2]
    ppr (Or e1 e2)   = sep $ punctuate (text ";") [ppr e1, ppr e2]
    ppr (Eq e1 e2)   = sep $ [ppr e1, text "=" , ppr e2]
    ppr (Not e1)     = text "not" <> parens (ppr e1)
    ppr e@(Lambda _ _) = parens (text "\\" <> sep (map ppr xs) <> text "^" <> ppr e')
        where (xs, e') = splitLambda e
--    ppr (Not e1)     = text "not" <> parens (ppr e1)
    ppr e@(Exists _ _) = parens (text "E" <> sep (map ppr xs) <+> ppr e')
        where (xs, e') = splitExist e
    ppr ListNil        = text "[]"
    ppr e@(ListCons _ _) = ppr_list (lst e)
        where ppr_list (es, Nothing) = brackets $ sep $ punctuate comma $ map ppr es
              ppr_list (es, Just e)  = brackets $ sep (punctuate comma $ map ppr es) <> text "|" <> ppr e
              lst (ListCons e1 ListNil) = ([e1], Nothing)
              lst (ListCons e1 e2) = ((e1:es), tail)
                where (es, tail) = lst e2
              lst e = ([], Just e)
--    ppr (Forall v e) = parens (text "F" <> ppr v <+> ppr e)
    ppr (Cut)        = text "!"

