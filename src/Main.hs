module Main where

import System.Console.GetOpt
import Interactive

{-
availopts =
[ Option ['i'] ["interactive"] (NoArg Interact) "go interactive"
, Option ['V'] ["version"]     (NoArg Version)  "show version"
, Option ['h'] ["help"]        (NoArg Help)     "show help"
]

-}

main = runInteract action
