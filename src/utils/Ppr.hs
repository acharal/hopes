module Ppr (
        Pretty(..),
        Message,
        pprint,
        module Text.PrettyPrint
    ) where

import Text.PrettyPrint

type Message = Doc

class Pretty a where
    ppr :: a -> Doc

pprint :: Pretty a => a -> IO ()
pprint a = print (ppr a)