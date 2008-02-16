module Symbol where

import Types

data Symbol a = Sym a | AnonSym

instance Eq a => Eq (Symbol a) where
    (Sym a) == (Sym b) = a == b
    _ == _ = False

instance Show a => Show (Symbol a) where
    showsPrec p (Sym a) = showsPrec p a
    showsPrec p AnonSym = showString "_"

liftSym :: a -> Symbol a
liftSym a = Sym a


type Sig a = ([a], [a])

emptySig = ([],[])
joinSig (a1,a2) (b1,b2) = (a1 ++ b1, a2 ++ b2)
rigSig s = ([s], [])
varSig s = ([], [s])

class HasSignature a s where
    sig :: a -> Sig s

instance HasSignature (Symbol a) (Symbol a) where
    sig (Sym s) = rigSig (Sym s)
    sig AnonSym = emptySig

symbols :: Sig s -> [s]
symbols (as, bs) = as ++ bs
vars    (as, bs) = bs
rigids  (as, bs) = as

{- 
specialSyms = 
 [ Special ":"  (TyFun (TyTup [tyAll, tyAll]) tyAll)
 , Special "[]" (tyAll)
 , Special "!"  (tyBool)
 , Special "s"  (TyFun tyAll tyAll)
 , Special "0"  (tyAll)
 ]
-}

buildinSym = [  liftSym ":",
                liftSym "[]",
                liftSym "!",
                liftSym "s",
                liftSym "0" ]

buildinTyp (Sym ":")  = TyFun (TyTup [tyAll, tyAll]) tyAll
buildinTyp (Sym "[]") = tyAll
buildinTyp (Sym "!" ) = tyBool
buildinTyp (Sym "s" ) = TyFun tyAll tyAll
buildinTyp (Sym "0" ) = tyAll
