module Hopl where

import Pretty
import Symbol
import Data.Monoid

data Expr a = 
      App (Expr a) (Expr a)
    | Set [Expr a] [a]
    | Flex a 
    | Rigid a 
    | Tup [Expr a]
    deriving Eq

type Clause a = (Expr a, [Expr a])

type Goal a = [Expr a]

type Prog a = [Clause a]

contradiction = []

isContra []    = True
isContra (_:_) = False

instance HasSignature (Expr a) a where
	sig (App e1 e2) = sig e1 `mappend` sig e2
	sig (Set _ _) = emptySig
	sig (Flex a)  = varSig a
	sig (Rigid a) = rigSig a
	sig (Tup es ) = mconcat $ map sig es

instance HasSignature (Clause a) a where
	sig (e1, e2) = sig e1 `mappend` sig e2

instance HasSignature (Goal a) a where
	sig gs = mconcat $ map sig gs

instance HasSignature (Prog a) a where
	sig cs = mconcat $ map sig cs


liftSet :: Expr a -> Expr a
liftSet e@(Set _ _) = e
liftSet (Flex v) = Set [] [v]
liftSet _ = error ("Cannot represent expression as set")


instance Pretty a => Pretty (Expr a) where
    ppr (Flex  sym)        = ppr sym
    ppr (Rigid sym)        = ppr sym
    ppr (Set es vs)         = curly  $ sep $ (punctuate comma (map ppr es)) ++ map (\v -> text "|" <+> ppr v) vs
    ppr (Tup es)           = parens $ sep $ (punctuate comma (map ppr es))
    ppr (App e e'@(Tup _)) = ppr e <> ppr e'
    ppr (App e e')         = ppr e <> parens (ppr e')

instance Pretty a => Pretty (Clause a) where
    ppr (h,b) = hang (sep [ppr h, entails]) 4 (sep (punctuate comma (map ppr  b)))

instance Pretty a => Pretty (Goal a) where
    ppr g = text "-?" <+> sep (punctuate comma (map ppr g))


instance Pretty a => Pretty (Prog a) where
    ppr p = vcat $ map ppr p
