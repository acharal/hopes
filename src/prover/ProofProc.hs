module ProofProc where

import Logic
import Hopl

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import List
import Pretty
import Char

import Debug.Trace

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

appT s (Var v)     = appV s v

appT s (Fun f tl)  = Fun f (map (appT s) tl)
appT s (Set tl vl) = 
    let fin1 = map (map (appT s)) tl
        terms = map (appV s) vl
        aux (Set tl vl) = (tl, vl)
        aux (Var v) = ([], [v])
        aux t = error ("you know")
        conctup (x, y) (z, w) = (x++z, y++w)
        (x, y) = foldl conctup ([],[]) (map aux terms)
        result = (Set (fin1 ++ x) y)
    in  result

appT s t           = t

appA s (Atom t tl) = Atom (appT s t) (map (appT s) tl)

varS :: Subst -> [Var]
varS s = map fst s

-- composition of substitutions
comp :: Subst -> Subst -> Subst
comp s s' =
    let s1 = filter (\(a,b) -> b /= (Var a)) [ (v, appT s' t) | (v, t) <- s ]
        s2 = filter (\(a,b) -> a `notElem` (varS s)) s'
    in  s1 ++ s2

-- restrict substitution to the set vl
restrict :: [Var] -> Subst -> Subst
restrict vl sl = filter (\(v,_) -> v `elem` vl) sl

-- Unification 

unify :: (Monad m) => Term -> Term -> m Subst
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

unify (Set tl []) (Set tl' []) = listUnify (concat tl) (concat tl')

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
listUnify lt lt' = fail ("Atoms " ++ (show lt) ++ 
                        " and "  ++ (show lt') ++
                        " are not unifyable")


occurCheck :: Var -> Term -> Bool
occurCheck v t = v `elem` (varsT t)


-- data InferEnv = InferEnv { prog :: Prog, tyenv :: TypeEnv }

type InferM = ReaderT Prog (StateT Int (LogicT Identity))

runInfer m p = runIdentity $ runL Nothing (evalStateT (runReaderT m p) 0)

isUserVar = isUpper.head

prove :: Goal -> InferM Subst
prove g = do
    subst <- refute g
    let answer = filter (isUserVar.fst) (restrict (varsAs g) subst)
    return answer

refute [] = return epsilon
refute g  =
    --trace ("Refute goal " ++ show g) $
    derive g                    >>- \(g',s') ->
    refute (map (appA s') g')   >>- \ans -> 
    return (s' `comp` ans)

clauses (Atom v _) = do
    prog <- ask
    let l = filter (\((Atom v' _), _) -> v' == v) prog
    msum (map return l)

derive :: Goal -> InferM (Goal, Subst)
derive [] = return (emptyGoal, epsilon)
derive g  =
    pickAtom g >>- \(a, g') -> 
    myresol a  >>- \(g'', s) ->
    return (g'' ++ g', s) 

pickAtom :: Goal -> InferM (Atom, Goal)
pickAtom []     = fail "Empty goal, cannot select an atom"
pickAtom (a:as) = return (a,as)

myresol a@(Atom (Pre _) _)  = resol a
myresol (Atom (Var v) args) = myresol (Atom (Set [] [v]) args)
myresol (Atom (Set tl (v:vl)) args) =
    basesOf args >>- \base_args -> 
    newVar >>- \v' -> 
    unify (Var v) (Set [base_args] [v']) >>- \subst -> 
    return ([], subst)

myresol a = error ("Not know what to do with atom " ++ show a ++" !")

-- the well-known resolution of first order case
resol a =
    clauses a          >>- \c ->
    variant c          >>- \(h',b') ->
    unifyAtoms a h'    >>- \subst ->
    return (b', subst)

basesOf [] = return []
basesOf (t:tl) = 
    mybases t >>- \b -> 
    basesOf tl >>- \bl -> 
    return (b:bl)

mybases (Pre p) = error ("Cannot *yet* enumerate the bases of predicate "++show p) -- refute p(X)
mybases t = return t



{-
baseElem :: Term -> [Term]
baseElem t = return t

baseElem2 :: Term -> InferM Subst -- return one or more substitution such that t `subst` s gives a base element (?)
baseElem2 (Var v) = 
baseElem2 (Pre p) = 
baseElem2 t = return epsilon -- no subst, term is itself base element
-}

{-
baseleq :: Term -> Term -> InferM Subst
baseleq (Var v) (Pre p) = 
    refute
baseleq (Var v) (Set tl [v']) =
    newVar >>- \v'' ->
    unify (Var v') (Set [(Var v)] [v''])
baseleq (Var v) t = unify (Var v) t
baseleq t1 t2 = error ("baseleq: undefined case!")
-}

variant c@(h,b) =
    let subst = mapM (\v -> newVar >>= \n -> return (v, (Var n))) (varsC c)
    in do 
        s <- subst
        return (appA s h, map (appA s) b)

newVar :: InferM Var
newVar = do
    i <- get
    modify (+1)
    return ("_G"++(show i))



instance Pretty Subst where
    ppr xs = vcat $ map ppr_bind xs
        where ppr_bind (v,t) = sep [ text v <+> text "=", ppr t ]
