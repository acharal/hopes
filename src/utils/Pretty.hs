module Pretty (
        Pretty(..),
        Message,
        pprint,
        dcolon, arrow, dot,
        module Text.PrettyPrint
    ) where

import Text.PrettyPrint

type Message = Doc

class Pretty a where
    ppr :: a -> Doc

pprint :: Pretty a => a -> IO ()
pprint a = print (ppr a)

dcolon = text "::"
arrow  = text "->"
dot    = char '.'
