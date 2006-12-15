module ProofProc where

-- import HOPL
import Control.Monad
import Control.Monad.State
import List
import Debug.Trace

-- Syntax of a Higher Order Program

type Var = String

data Term =
    Var Var             -- variable
  | Pre Var             -- predicate
  | Con String          -- constant of type i
  | Fun Var [Term]      -- function symbol
  | Uni Term Term       -- union of sets
  | Set [Term]          -- a set of terms
 deriving Eq

data Atom = Atom Term [Term]
 deriving Eq

type Clause = (Atom, [Atom])

type Prog = [Clause]

type Goal = [Atom]

instance Show Term where
    showsPrec n (Var v) = showString v
    showsPrec n (Pre v) = showString v
    showsPrec n (Con c) = showString c
    showsPrec n (Fun v tl) = (showString v).(showString "(").(showTerms tl).(showString ")")
    showsPrec n (Set tl)   = (showString "{").(showTerms tl).(showString "}")
    showsPrec n (Uni t t') = (showsPrec n t).(showString "U").(showsPrec n t')

instance Show Atom where
    showsPrec n (Atom t tl) = (shows t).(showString "(").(showTerms tl).(showString ")")

showTerms []  = id
showTerms [t] = (shows t)
showTerms (t:tl) = (shows t).(showString ", ").(showTerms tl)

emptyGoal :: Goal
emptyGoal = []

varsT :: Term -> [Var]
varsT (Var v)    = [v]
varsT (Fun _ tl) = nub (concatMap varsT tl)
varsT (Set tl)   = nub (concatMap varsT tl)
varsT (Uni t t') = (varsT t) ++ (varsT t')
varsT _          = []

varsA :: Atom -> [Var]
varsA (Atom (Var v) tl) = nub (v:(concatMap varsT tl))
varsA (Atom (Pre v) tl) = nub (concatMap varsT tl)
varsA _ = error "Not expected atom"

varsAs atoms = nub (concatMap varsA atoms)

varsC :: Clause -> [Var]
varsC (h,b) = varsAs (h:b)

-- Substitutions 

type Subst = [(Var, Term)]  -- substitution list

epsilon :: Subst        -- identity substitution
epsilon = []


-- applying a substitution to various structures

appV :: Subst -> Var -> Term
appT :: Subst -> Term -> Term
appA :: Subst -> Atom -> Atom

appV [] v = Var v
appV ((v',t'):ss) v
    | v' == v   = t'
    | otherwise = appV ss v

appT s (Var v)    = appV s v

appT s (Fun f tl) = Fun f (map (appT s) tl)
appT s (Set tl)   = Set (map (appT s) tl)
appT s (Uni t t') = Uni (appT s t) (appT s t')
appT s t          = t

appA s (Atom t tl)= Atom (appT s t) (map (appT s) tl)

varS :: Subst -> [Var]
varS s = map fst s

-- composition of substitutions
comp :: Subst -> Subst -> Subst
comp s s' =
    let s1 = filter (\(a,b) -> b /= (Var a)) [ (v, appT s' t) | (v, t) <- s ]
        s2 = filter (\(a,b) -> a `notElem` (varS s)) s'
    in  s1 ++ s2


restrict :: [Var] -> Subst -> Subst
restrict vl sl = filter (\(v,_) -> v `elem` vl) sl

-- Unification 

--unify :: Term -> Term -> Maybe Subst
unify (Var v) (Var v')
    | v == v'   = return epsilon
    | otherwise = return [(v, Var v')]

unify (Var v) t = 
    if occurCheck v t then
        fail ("Variable "   ++ (show v) ++
              " occurs in " ++ (show t) )
    else
        return [(v,t)]

unify t (Var v) = unify (Var v) t

unify (Fun f tl) (Fun f' tl')
    | f == f'   = listUnify tl tl'
    | otherwise = fail ""

unify (Set tl) (Set tl') = listUnify tl tl'

unify t t' 
    | t == t'   = return epsilon
    | otherwise = fail ("Terms " ++ (show t) ++ 
                        " and "  ++ (show t') ++
                        " are not unifyable")

unifyAtoms a1@(Atom p pl) a2@(Atom q ql)
    | p == q    = listUnify pl ql
    | otherwise = fail ("Atoms " ++ (show a1) ++ 
                        " and "  ++ (show a2) ++
                        " are not unifyable")

-- listUnify :: [Term] -> [Term] -> Maybe Subst
listUnify [] [] = return epsilon
listUnify (x:xs) (y:ys) =  do
    s  <- unify x y
    s' <- listUnify (map (appT s) xs) (map (appT s) ys)
    return (s `comp` s')


occurCheck :: Var -> Term -> Bool
occurCheck v t = v `elem` (varsT t)


-- Backtrack Monad

newtype BackTr a = Stream { unStream :: [a] }

type Refutation a = StateT Int BackTr a

instance Monad BackTr where
    return a         = Stream [a]
    (Stream l) >>= f = Stream $ concatMap (unStream.f) l
    --s >>= f = s >>- f
    fail str         = Stream []

--(Stream l) >>== f = Stream $ concatMap (unStream.f) l

instance MonadPlus BackTr where
    mzero = Stream []
    mplus (Stream a) (Stream b) = Stream $ a ++ b

--msplit :: BackTr a -> BackTr (Maybe (a, BackTr a))
msplit (Stream [])     = return $ Nothing
msplit (Stream (a:as)) = return $ Just (a, Stream as)

interleave s s' = do
    msplit s >>= \r -> case r of
                            Nothing -> s'
                            Just (a,as) -> (return a) `mplus` (interleave s' as)

s >>- g = do
    msplit s >>= \r -> case r of
                            Nothing -> mzero
                            Just (a,as) -> (g a) `interleave` (as >>- g)


runL :: Maybe Int -> BackTr a -> [a]
runL (Just n) (Stream s) = take n s
runL Nothing  (Stream s) = s

-- refute :: Prog -> Goal -> BackTr Subst
refute p [] = return epsilon
refute p g  =
    resol p g >>= \(g',s') ->
    refute p (map (appA s') g') >>= \ans -> 
    return (s' `comp` ans)

prove p g = do
    subst <- refute p g
    return (restrict (varsAs g) subst)

--clauses :: Var -> Prog -> BackTr Clause
clauses p (Atom v _) = msum (map return (filter (\((Atom v' _), _) -> v' == v) p))

selectAtom []     = fail "Empty goal, cannot select an atom"
selectAtom (a:as) = return (a,as)

-- resol :: Prog -> Goal -> BackTr (Goal,Subst)
resol p [] = return (emptyGoal,epsilon)
resol p g  = do
    (a,rest) <- selectAtom g
    c        <- clauses p a
    (h',b')  <- variant c
    subst    <- unifyAtoms a h'
    return (b'++rest, subst)

--variant :: Clause -> Refutation Clause
variant c@(h,b) =
    let subst = mapM (\v -> newVar >>= \n -> return (v, (Var n))) (varsC c)
    in do 
        s <- subst
        return (appA s h, map (appA s) b)

newVar :: Refutation Var
newVar = do
    i <- get
    put (i+1)
    return ("_G"++(show i))
