module Syntax where

import Types
import Loc

{-
 - Concrete syntax tree for the surface language.
 - Expressions are polymorphic to include extra information,
 - which will varry depending on the context.
 -}

-- Description of constants and variables.
data Const a = Const a String -- info and name 
    deriving Functor

data Var a = Var a String -- named variable
           | AnonVar a    -- wildcard
    deriving Functor


-- The whole program
type SProgram a = [SGroup a]

-- A dependency group
type SGroup a = [SClause a]

-- A clause. Nothing in the body is a fact. Just is a rule.
data SClause a = SClause a (SHead a) ( Maybe (SGets, SExpr a) )
    deriving Functor

-- Monomorphic or polymorphic gets
-- TODO implement polymorphic
data SGets = SGets_mono | SGets_poly 

isFact :: SClause a -> Bool
isFact (SClause _ _ Nothing) = True
isFact _ = False

data SHead a = SHead a           -- Info
                     ( Const a ) -- clause name
                     [[SExpr a]] -- Arguments
    deriving Functor
{-
-- Simple expressions describe the arguments of a head of a rule
data SSimple a = SSimple_const a (Const a)
               | SSimple_var   a (Var   a)
               | SSimple_int   a (Integer)
               | SSimple_float a (Double )
               | SSimple_app   a (Const a) [SSimple a] 
                    -- simple application
               | SSimple_op 
               | SSimple_paren a (SSimple a)
-}

{-
data SBody a = SBody_par a (SBody a)    -- in parens
             | SBody_pop a (SPOp a) (SBody a) (SBody a) 
                                        -- operator
             | SBody_pe  a (SExpr a)    -- pred. expr
    deriving Functor
-}
{-
data SPOp a = SAnd a | SOr a | SFollows a
    deriving Functor
-}
data SExpr a = SExpr_paren   a (SExpr a)  -- in Parens    
             | SExpr_const   a            -- constant 
                             (Const a)    -- id
                             Bool         -- interpreted as predicate?
                             (Maybe Integer)  -- optional GIVEN arity 
                             (Maybe Integer)  -- INFERRED arity
             | SExpr_var     a (Var a)    -- variable
             | SExpr_int     a Integer    -- integer constant
             | SExpr_float   a Double     -- Floating point constant
             | SExpr_predCon a            -- predicate constant
                             (Const a)    -- id
                             (Maybe Integer)  -- optional GIVEN arity
                             (Maybe Integer)  -- INFERRED arity
             | SExpr_app  a (SExpr a) [SExpr a]  -- application
             | SExpr_op   a (Const a) [SExpr a]  -- operator
             | SExpr_lam  a [Var a] (SExpr a)    -- lambda abstr.
             | SExpr_list a [SExpr a] (Maybe (SExpr a))
                          -- list: initial elements, maybe tail
             | SExpr_eq   a (SExpr a) (SExpr a)  -- unification
             | SExpr_ann  a (SExpr a) Type       -- type annotated
    deriving Functor

data SGoal a = SGoal a (SExpr a)
    deriving Functor

data SCommand a = SCommand a (SExpr a)
    deriving Functor

data SSent a = SSent_clause a (SClause  a)
             | SSent_goal   a (SGoal    a)
             | SSent_comm   a (SCommand a)
    deriving Functor

-- Print expressions
-- TODO make them better
deriving instance Show a => Show (Const a)
deriving instance Show a => Show (Var a)
deriving instance Show a => Show (SClause a)
deriving instance Show SGets
deriving instance Show a => Show (SHead a)
deriving instance Show a => Show (SExpr a)
--deriving instance Show a => Show (SBody a)
--deriving instance Show a => Show (SPOp a)
deriving instance Show a => Show (SGoal a)
deriving instance Show a => Show (SCommand a)
deriving instance Show a => Show (SSent a)


{-
-- Syntax constructs have types if it exists in the 
-- information they carry
-- Maybe useful later
instance HasType a => HasType (Const a) where
    typeOf (Const a _ _) = typeOf a
    hasType tp (Const a s i) = Const (hasType tp a) s i

instance HasType a => HasType (Var a) where
    typeOf (Var a _)   = typeOf a
    typeOf (AnonVar a) = typeOf a
    hasType tp (Var a s)   = Var (hasType tp a) s
    hasType tp (AnonVar a) = AnonVar (hasType tp a)
-- FIXME : complete these!
instance HasType a => HasType (SClause a) where
    typeOf (SClause a _ _ _) = typeOf a 
    --hasType tp (SClause a _ _ _) = hasType tp a

instance HasType a => HasType (SHead a) where
    typeOf (SHead a _ _) = typeOf a
    --hasType tp (SHead a _ _) = hasType tp a

instance HasType a => HasType (SBody a) where
    typeOf (SBody_par a _ )     = typeOf a
    typeOf (SBody_pop a _ _ _ ) = typeOf a
    typeOf (SBody_pe  a _ )     = typeOf a
    --hasType tp (SBody_par a _ )     = hasType tp a
    --hasType tp (SBody_pop a _ _ _ ) = hasType tp a
   -- hasType tp (SBody_pe  a _ )     = hasType tp a

instance HasType a => HasType (SOp a) where
    typeOf (SAnd a) = typeOf a
    typeOf (SOr  a) = typeOf a
    typeOf (SFollows a) = typeOf a
  --  hasType tp (SAnd a) = hasType tp a
   -- hasType tp (SOr  a) = hasType tp a
  --  hasType tp (SFollows a) = hasType tp a

instance HasType a => HasType (SExpr a) where
    typeOf (SExpr a _ _ ) = typeOf a
  --  hasType tp (SExpr a _ _ ) = hasType tp a

instance HasType a => HasType (SGoal a) where
    typeOf (SGoal a ) = typeOf a
  --  hasType tp (SGoal a) = hasType tp a

-- Syntax constructs have location if it exists in the 
-- information they carry
-- Maybe useful later
instance HasLocation a => HasLocation (Const a) where
    loc (Const a _ _) = loc a
    
instance HasLocation a => HasLocation (Var a) where
    loc (Var a _)   = loc a
    loc (AnonVar a) = loc a

instance HasLocation a => HasLocation (SClause a) where
    loc (SClause a _ _ _) = loc a

instance HasLocation a => HasLocation (SHead a) where
    loc (SHead a _ _) = loc a

instance HasLocation a => HasLocation (SBody a) where
    loc (SBody_par a _ )     = loc a
    loc (SBody_pop a _ _ _ ) = loc a
    loc (SBody_pe  a _ )     = loc a

instance HasLocation a => HasLocation (SOp a) where
    loc (SAnd a) = loc a
    loc (SOr  a) = loc a
    loc (SFollows a) = loc a

instance HasLocation a => HasLocation (SExpr a) where
    loc (SExpr a _ _ ) = loc a

instance HasLocation a => HasLocation (SGoal a) where
    loc (SGoal a) = loc a
-}

-- true and fail constants

sTrue :: SExpr ()
sTrue =  SExpr_const () 
                     (Const () "true")  
                     True
                     Nothing
                     (Just 0)
sFail :: SExpr ()
sFail =  SExpr_const () 
                     (Const () "fail")  
                     True
                     Nothing
                     (Just 0)

sCut :: SExpr ()
sCut =  SExpr_const () 
                    (Const () "!")  
                    True
                    Nothing
                    (Just 0)

sNil :: SExpr ()
sNil = SExpr_const () 
                   (Const () "[]")  
                   False
                   Nothing
                   (Just 0)

