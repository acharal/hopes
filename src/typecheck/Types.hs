module Types where

data TermTyp = 
      Base
    | Func [TermTyp]

data PredTyp = 
      Pred [TermTyp]

data Typ = 
      PredT [TermTyp]

