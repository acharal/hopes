module Pretty (
        Pretty(..),
        pprint,
        dcolon, arrow, dot, entails,
        module Text.PrettyPrint
    ) where

import Text.PrettyPrint

class Pretty a where
    ppr :: a -> Doc

instance Pretty Doc where
    ppr = id

dcolon  = text "::"
arrow   = text "->"
dot     = char '.'
entails = text ":-"

pprint :: Pretty a => a -> IO ()
pprint a = print (ppr a)
