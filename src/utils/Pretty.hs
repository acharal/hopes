module Pretty (
        module Pretty,
        module Text.PrettyPrint
    ) where

import Text.PrettyPrint

class Pretty a where
    ppr :: a -> Doc

instance Pretty Doc where
    ppr = id

instance Pretty [Char] where
    ppr = text

instance Pretty Int where
    ppr = int

dcolon  = text "::"
arrow   = text "->"
dot     = char '.'
entails = text ":-"

pprint :: Pretty a => a -> IO ()
pprint a = print (ppr a)

curly a = text "{" <+> a <+> text "}"
