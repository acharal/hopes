
module Subst.Pretty where

import Pretty
import Subst
import Language.Hopl
import Language.Hopl.Pretty ()
import Lang


instance (Pretty a, Eq a, Symbol a, HasLogicConstants (Expr a)) => Pretty (Subst a) where
    ppr xs = vcat $ map ppr_bind xs
        where ppr_bind (v,t) = sep [ ppr v <+> text "=", ppr t ]

class Pretty a => AnswerPrintable a where
    printanswer :: a -> Doc

instance (Symbol a, Pretty a, Eq a) => AnswerPrintable (Subst a) where
    printanswer xs = vcat $ map ppr_bind xs
        where ppr_bind (v,t) = sep [ ppr v <+> text "=", printanswer t ]

instance (Symbol a, Pretty a, Eq a) =>  AnswerPrintable (Expr a) where
    printanswer a = ppr a 
