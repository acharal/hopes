module Syntax where

import Types
import Loc

{-
 - Concrete syntax tree for the surface language.
 - Expressions are polymorphic to include extra information,
 - which will varry depending on the context.
 -}

-- Description of constants and variables.
-- Extra type argument for information (type,location etc)
data Const a = Const a String Int  -- name and arity
    deriving Functor

data Var   a = Var     a String -- named Variable
             | AnonVar a        -- wildcard
    deriving Functor

-- The whole program
type SProgram a = [SGroup a]

-- A dependency group
type SGroup a = [SClause a]

-- A clause. Nothing in the body is a fact. Just is a rule.
data SClause a = SClause a (SHead a) (SGets) (Maybe (SBody a))
    deriving Functor

-- Monomorphic or polymorphic gets
data SGets = SGets_mono | SGets_poly

isFact :: SClause a -> Bool
isFact (SClause _ _ _ Nothing) = True
isFact _ = False

data SHead a = SHead a           -- Info
                     ( Const a ) -- clause name
                     [[SExpr a]] -- Arguments
    deriving Functor

data SBody a = SBody_par a (SBody a)    -- in parens
             | SBody_pop  a (SOp a) (SBody a) (SBody a) 
                                        -- operator
             | SBody_pe  a (SExpr a)    -- pred. expr
    deriving Functor

data SOp a = SAnd a | SOr a | SFollows a
    deriving Functor

data SExpr a = SExpr a       -- Info
                     Bool    -- Is it predicative expr?
                     (SContents a) -- Contents
    deriving Functor

data SContents a = SCont_par     (SExpr a)  -- in Parens    
                 | SCont_const   (Const a)  -- constant
                 | SCont_var     (Var   a)  -- variable
                 | SCont_int     Int        -- integer constant
                 | SCont_predcon (Const a)  -- predicate constant
                 | SCont_app  (SExpr a) [SExpr a] -- application
                 | SCont_lam  [Var a]   (SBody a) -- lambda abstr.
                 | SCont_list [SExpr a] (Maybe (SExpr a))
                              -- list: initial elements, maybe tail
                 | SCont_eq  (SExpr a) (SExpr a)  -- unification
                 | SCont_ann (SExpr a) Type       -- type annotaded
    deriving Functor

data SGoal a = SGoal (SBody a)
    deriving Functor

-- Syntax constructs have types if it exists in the 
-- information they carry
-- Maybe useful later
instance HasType a => HasType (Const a) where
    typeOf (Const a _ _) = typeOf a
    
instance HasType a => HasType (Var a) where
    typeOf (Var a _)   = typeOf a
    typeOf (AnonVar a) = typeOf a

instance HasType a => HasType (SClause a) where
    typeOf (SClause a _ _ _) = typeOf a

instance HasType a => HasType (SHead a) where
    typeOf (SHead a _ _) = typeOf a

instance HasType a => HasType (SBody a) where
    typeOf (SBody_par a _ )     = typeOf a
    typeOf (SBody_op  a _ _ _ ) = typeOf a
    typeOf (SBody_pe  a _ )     = typeOf a

instance HasType a => HasType (SOp a) where
    typeOf (SAnd a) = typeOf a
    typeOf (SOr  a) = typeOf a
    typeOf (SFollows a) = typeOf a

instance HasType a => HasType (SExpr a) where
    typeOf (SExpr a _ _ ) = typeOf a

instance HasType a => HasType (SGoal a) where
    typeOf (SGoal a) = typeOf a

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
    loc (SBody_op  a _ _ _ ) = loc a
    loc (SBody_pe  a _ )     = loc a

instance HasLocation a => HasLocation (SOp a) where
    loc (SAnd a) = loc a
    loc (SOr  a) = loc a
    loc (SFollows a) = loc a

instance HasLocation a => HasLocation (SExpr a) where
    loc (SExpr a _ _ ) = loc a

instance HasLocation a => HasLocation (SGoal a) where
    loc (SGoal a) = loc a
