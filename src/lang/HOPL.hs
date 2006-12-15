module HOPL where

-- Higher Order Predicate Language (possibly the Core language of the system)

type Var   = String
type Const = String

data HoTerm   = 
     HoVar   Var                    -- variables
   | HoConst Const                  -- constant
   | HoFun   Var [HoTerm]           -- functional symbol
  deriving (Show)

data HoAtom   = HoAtom Var [HoTerm]
  deriving (Show)


type HoClause = (HoAtom, [HoAtom])  -- facts can be represented as clauses with empty body

type HoGoal   = [HoAtom]            -- clause without head

type HoProg   = [HoClause]

