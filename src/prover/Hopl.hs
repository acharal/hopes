module Hopl where

import Pretty

data Expr a = 
      App (Expr a) (Expr a)
    | Set [Expr a] a
    | Flex a 
    | Rigid a 
    | Tup [Expr a]
    deriving Eq

type Clause a = (Expr a, [Expr a])

type Goal a = [Expr a]

type Prog a = [Clause a]

contradiction = []

flexs  :: Expr a -> [a]
flexs (Flex a)   = [a]
flexs (App e e') = flexs e ++ flexs e'
flexs (Set el v) = v : concatMap flexs el
flexs (Tup es)   = concatMap flexs es
flexs _ = []


instance Pretty a => Pretty (Expr a) where
    ppr (Flex  sym)        = ppr sym
    ppr (Rigid sym)        = ppr sym
    ppr (Set es v)         = curly  $ sep $ (punctuate comma (map ppr es)) ++ [text "|" <+> ppr v]
    ppr (Tup es)           = parens $ sep $ (punctuate comma (map ppr es))
    ppr (App e e'@(Tup _)) = ppr e <> ppr e'
    ppr (App e e')         = ppr e <> parens (ppr e')

instance Pretty a => Pretty (Clause a) where
    ppr (h,b) = hang (sep [ppr h, entails]) 4 (sep (punctuate comma (map ppr  b)))

instance Pretty a => Pretty (Goal a) where
    ppr g = text "-?" <+> sep (punctuate comma (map ppr g))


instance Pretty a => Pretty (Prog a) where
    ppr p = vcat $ map ppr p
