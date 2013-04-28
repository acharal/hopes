import Data.Maybe (fromJust)
import Data.List (nub)

-- Syntax

newtype V = V String
  deriving (Eq, Show)
newtype C = C String
  deriving (Eq, Show)

data E  =  E_var V
        |  E_con C
        |  E_predcon C Int
        |  E_app E [E]
        |  E_lam [V] E
        |  E_and E E
        |  E_or E E
        |  E_eq E E
        |  E_ex V E
  deriving Show

newtype R = R (C, Int, E)
  deriving Show

type G = [R]
type P = [G]

-- Types

newtype Alpha = Alpha String
  deriving Eq
newtype Phi = Phi String
  deriving Eq

data RTyp = R_iota | R_t Typ | R_var Alpha
  deriving Eq
data Typ = T_o | T_fun [RTyp] Typ | T_var Phi
  deriving Eq
data ETyp = ET_iota | ET_var Alpha | ET_poly [Alpha] [Phi] Typ
  deriving Eq

mono :: RTyp -> ETyp
mono R_iota = ET_iota
mono (R_var alpha) = ET_var alpha
mono (R_t pi) = ET_poly [] [] pi

isMono :: ETyp -> RTyp
isMono ET_iota = R_iota
isMono (ET_var alpha) = R_var alpha
isMono (ET_poly [] [] pi) = R_t pi
isMono et = error $ "monomorphism violation:\n  " ++ show et

-- Environments

type Env = [(Either (C, Int) V, ETyp)]

-- A state monad for generating unique variable names

newtype T a = T (Int -> (a, Int))

instance Monad T where
  return x   =  T (\n ->  (x, n))
  T m >>= f  =  T (\n ->  let  (x, n') = m n
                               T m' = f x
                          in   m' n')

runT :: T a -> a
runT (T m) = fst (m 1)

newAlpha :: T Alpha
newAlpha = T (\n -> (Alpha ('a' : show n), n+1))

newAlphas :: Int -> T [Alpha]
newAlphas n = sequence $ take n $ repeat $ newAlpha

newPhi :: T Phi
newPhi = T (\n -> (Phi ('t' : show n), n+1))

newPhis :: Int -> T [Phi]
newPhis n = sequence $ take n $ repeat $ newPhi

-- Typing

type Constraint = (RTyp, RTyp)

typP :: P -> Env -> Env
typP [] gamma = gamma
typP (g : gs) gamma =
  let  gamma' = typG g gamma
  in   typP gs (gamma ++ gamma')

typG :: G -> Env -> Env
typG g gamma = runT $ do
  let arrtyp c n
        | n == 0     =  do  phi <- newPhi
                            let rho = R_t (T_var phi)
                            return $ (Left (c, n), mono rho)
        | otherwise  =  do  alphas <- newAlphas n
                            phi <- newPhi
                            let rho = R_t (T_fun (map R_var alphas) (T_var phi))
                            return $ (Left (c, n), mono rho)
  gamma' <- sequence [arrtyp c n | R (c, n, e) <- g]
  cstrlist <- sequence [typR r (gamma ++ gamma') | r <- g]
  let s = unify $ concat cstrlist
  return $ generalize $ subst s gamma'

typR :: R -> Env -> T [Constraint]
typR (R (c, n, e)) gamma = do
  let R_t pi = isMono $ fromJust $ lookup (Left (c, n)) gamma
  (rho, cstr) <- typE e gamma
  return $ (R_t pi, rho) : cstr

typE :: E -> Env -> T (RTyp, [Constraint])
typE (E_var v) gamma = do
  let rho = isMono $ fromJust $ lookup (Right v) gamma
  return (rho, [])
typE (E_con c) gamma = do
  return (R_iota, [])
typE (E_predcon c n) gamma = do
  let t = fromJust $ lookup (Left (c, n)) gamma
  pi <- freshen t
  return (R_t pi, [])
typE (E_app (E_con c) es) gamma = do
  rcs <- sequence [typE e gamma | e <- es]
  let (rhos, cstrlist) = unzip rcs
  return (R_iota, [(rho, R_iota) | rho <- rhos] ++ concat cstrlist)
typE (E_app e es) gamma = do
  (rho, cstr) <- typE e gamma
  rcs <- sequence [typE e gamma | e <- es]
  let (rhos, cstrlist) = unzip rcs
  phi <- newPhi
  let pi = T_var phi
  return (R_t pi, (rho, R_t (T_fun rhos pi)) : cstr ++ concat cstrlist)
typE (E_lam vs e) gamma = do
  alphas <- newAlphas (length vs)
  let gamma' = [(Right v, ET_var alpha) | (v, alpha) <- zip vs alphas] ++ gamma
  (rho, cstr) <- typE e gamma'
  phi <- newPhi
  let rho' = R_t (T_fun (map R_var alphas) (T_var phi))
  return (rho', (R_t (T_var phi), rho) : cstr)
typE (E_and e1 e2) gamma = do
  (rho1, cstr1) <- typE e1 gamma
  (rho2, cstr2) <- typE e2 gamma
  phi <- newPhi
  let rho' = R_t (T_var phi)
  return (rho', (rho', rho1) : (rho', rho2) : cstr1 ++ cstr2)
typE (E_or e1 e2) gamma = do
  (rho1, cstr1) <- typE e1 gamma
  (rho2, cstr2) <- typE e2 gamma
  phi <- newPhi
  let rho' = R_t (T_var phi)
  return (rho', (rho', rho1) : (rho', rho2) : cstr1 ++ cstr2)
typE (E_eq e1 e2) gamma = do
  (rho1, cstr1) <- typE e1 gamma
  (rho2, cstr2) <- typE e2 gamma
  return (R_t T_o, (R_iota, rho1) : (R_iota, rho2) : cstr1 ++ cstr2)
typE (E_ex v e) gamma = do
  alpha <- newAlpha
  let gamma' = (Right v, ET_var alpha) : gamma
  (rho, cstr) <- typE e gamma'
  -- For monomorphic exists, here's what you need:
  -- return (R_t T_o, (rho, R_t T_o) : cstr)
  phi <- newPhi
  let rho' = R_t (T_var phi)
  return (rho', (rho', rho) : cstr)

-- Unification

type Subst = RTyp -> RTyp

substAlpha alpha rho rho'@(R_var alpha')
  | alpha == alpha'  =  rho
  | otherwise        =  rho'
substAlpha alpha rho (R_t pi) = R_t (aux alpha rho pi)
  where aux alpha rho (T_fun rhos pi) =
          T_fun (map (substAlpha alpha rho) rhos) (aux alpha rho pi)
        aux alpha rho pi = pi
substAlpha alpha rho rho' = rho'

substPhi phi t (R_t t') = R_t (aux phi t t')
  where aux phi t t'@(T_var phi') | phi == phi' = t
        aux phi t (T_fun rhos pi) =
          T_fun (map (substPhi phi t) rhos) (aux phi t pi)
        aux phi t t' = t'
substPhi phi t phi' = phi'

everywhere :: Subst -> [Constraint] -> [Constraint]
everywhere s cstr = [(s rho1, s rho2) | (rho1, rho2) <- cstr]

subst :: Subst -> Env -> Env
subst s gamma = [(x, mono (s rho)) | (x, et) <- gamma, let rho = isMono et]

unify :: [Constraint] -> Subst
unify [] =
  id
unify ((rho1, rho2) : cstr) | rho1 == rho2 =
  unify cstr
unify ((R_var alpha, rho2) : cstr) | not $ alpha `elem` fvr rho2 =
  let  s = substAlpha alpha rho2
  in   unify (everywhere s cstr) . s
unify ((rho1, R_var alpha) : cstr) | not $ alpha `elem` fvr rho1 =
  let  s = substAlpha alpha rho1
  in   unify (everywhere s cstr) . s
unify ((R_t (T_var phi), R_t t2) : cstr) | not $ phi `elem` fvt t2 =
  let  s = substPhi phi t2
  in   unify (everywhere s cstr) . s
unify ((R_t t1, R_t (T_var phi)) : cstr) | not $ phi `elem` fvt t1 =
  let  s = substPhi phi t1
  in   unify (everywhere s cstr) . s
unify ((R_t (T_fun rhos1 pi1), R_t (T_fun rhos2 pi2)) : cstr)
  | length rhos1 == length rhos2 =
    unify (zip rhos1 rhos2 ++ [(R_t pi1, R_t pi2)] ++ cstr)
unify ((rho1, rho2) : cstr) =
  error $ "unification failed:\n  " ++ show rho1 ++ "\n  " ++ show rho2

generalize :: Env -> Env
generalize gamma = [(x, gen t) | (x, t) <- gamma]

gen :: ETyp -> ETyp
gen ET_iota = ET_iota
gen (ET_poly [] [] pi) = ET_poly (fvr (R_t pi)) (fvt pi) pi
gen et = error $ "generalization violation:\n  " ++ show et

freshen :: ETyp -> T Typ
freshen (ET_poly alphas phis pi) = do
  alphas' <- newAlphas (length alphas)
  phis' <- newPhis (length phis)
  let ss = [substAlpha alpha (R_var alpha') |
             (alpha, alpha') <- zip alphas alphas'] ++
           [substPhi phi (T_var phi) |
             (phi, phi') <- zip phis phis']
  let s = foldl (.) id ss
  let R_t pi' = s (R_t pi)
  return pi'
freshen et = error $ "freshening violation:\n  " ++ show et

fvr :: RTyp -> [Alpha]
fvr (R_var alpha) = [alpha]
fvr (R_t (T_fun rhos pi)) = nub $ concatMap fvr rhos
fvr rho = []

fvt :: Typ -> [Phi]
fvt (T_var phi) = [phi]
fvt (T_fun rhos pi) = nub $ concatMap fvtr rhos ++ fvt pi
  where fvtr (R_t t) = fvt t
        fvtr rho = []
fvt t = []

-- Pretty printing for types

instance Show Alpha where
  showsPrec p (Alpha alpha) = (alpha ++)

instance Show Phi where
  showsPrec p (Phi phi) = (phi ++)

instance Show RTyp where
  showsPrec p R_iota = ("i" ++)
  showsPrec p (R_t pi) = showsPrec p pi
  showsPrec p (R_var alpha) = showsPrec p alpha

instance Show Typ where
  showsPrec p T_o = ("o" ++)
  showsPrec p (T_fun rhos pi) =
    ("(" ++) . walk rhos . (") -> " ++) . showsPrec p pi
    where walk [rho] = showsPrec p rho
          walk (rho : rhos) = showsPrec p rho . (", " ++) . walk rhos
  showsPrec p (T_var phi) = showsPrec p phi

instance Show ETyp where
  showsPrec p ET_iota  = ("i" ++)
  showsPrec p (ET_var alpha) = showsPrec p alpha
  showsPrec p (ET_poly alphas phis pi) =
    walk alphas . walk phis . showsPrec p pi
    where walk [] = id
          walk (x : xs) = ("∀" ++) . showsPrec p x . (". " ++) . walk xs

-- Examples

t :: P -> IO ()
t p = walk $ typP p gamma0
  where  walk [] = return ()
         walk ((Left (C c, n), et) : gamma) = do
           putStr $ c ++ "/" ++ show n ++ " :: "
           print et
           walk gamma

gamma0 :: Env
gamma0 = [(Left (C "true", 0), ET_poly [] [] T_o)]

true :: E
true = E_predcon (C "true") 0

ex1 =
  [
    R (C "a", 0, true)
  ] :
  [
    R (C "a", 1, E_lam [V "X"] $ E_eq (E_var (V "X")) (E_con (C "0")))
  ] :
  [
    R (C "b1", 1, E_lam [V "X"] $ E_app (E_var (V "X")) [E_con (C "a")])
  ] :
  [
    R (C "b2", 1, E_lam [V "X"] $ E_app (E_var (V "X")) [E_predcon (C "a") 0])
  ] :
  [
    R (C "b3", 1, E_lam [V "X"] $ E_app (E_var (V "X")) [E_predcon (C "a") 1])
  ] :
  []

ex2 =
  [
    R (C "map", 1, E_lam [V "R"] $
                   E_lam [V "L1", V "L2"] $
                   (E_eq (E_var (V "L1")) (E_con (C "[]"))
                    `E_and`
                    E_eq (E_var (V "L2")) (E_con (C "[]")))
                   `E_or`
                   (E_ex (V "H1") $
                    E_ex (V "L1") $
                    E_ex (V "H2") $
                    E_ex (V "L2") $
                    E_eq (E_var (V "L1"))
                      (E_app (E_con (C "|")) [E_var (V "H1"), E_var (V "L1")])
                    `E_and`
                    E_eq (E_var (V "L2"))
                      (E_app (E_con (C "|")) [E_var (V "H2"), E_var (V "H2")])
                    `E_and`
                    E_app (E_var (V "R")) [E_var (V "H1"), E_var (V "H1")]
                    `E_and`
                    E_app (E_app (E_predcon (C "map") 1) [E_var (V "R")])
                          [E_var (V "L1"), E_var (V "L1")]))
  ] :
  []

ex3 =
  [
    R (C "foo", 1, E_lam [V "X"] $
                   E_app (E_predcon (C "bar") 1) [E_var (V "X")]),
    R (C "bar", 1, E_lam [V "X"] $
                   E_app (E_predcon (C "foo") 1)
                         [E_app (E_con (C "s")) [E_var (V "X")]])
  ] :
  []

ex4 =
  [
    R (C "apply", 2, E_lam [V "X", V "Y"] $
                     E_app (E_var (V "X")) [E_var (V "Y")])
  ] :
  [
    R (C "isZero", 1, E_lam [V "X"] $
                      E_app (E_predcon (C "apply") 2)
                      [E_lam [V "Y"] $
                       E_eq (E_var (V "Y")) (E_con (C "0")),
                       E_var (V "X")])
  ] :
  []

ex4curried =
  [
    R (C "apply", 1, E_lam [V "X"] $
                     E_lam [V "Y"] $
                     E_app (E_var (V "X")) [E_var (V "Y")])
  ] :
  [
    R (C "isZero", 1, E_app (E_predcon (C "apply") 1)
                      [E_lam [V "Y"] $
                       E_eq (E_var (V "Y")) (E_con (C "0"))])
  ] :
  []

ex5 =
  [
    R (C "foo", 1, E_lam [V "X"] $
                   E_app (E_predcon (C "bar") 1) [E_var (V "X")]),
    R (C "bar", 1, E_lam [V "X"] $
                   E_app (E_predcon (C "foo") 1) [E_var (V "X")])
  ] :
  [
    R (C "baz", 1, E_lam [V "X"] $
                   E_app (E_predcon (C "foo") 1)
                         [E_app (E_con (C "s")) [E_var (V "X")]]
                   `E_and`
                   E_app (E_predcon (C "bar") 1) [E_var (V "X")])
  ] :
  []

ex6 = 
  [
    R (C "closure", 1, 
        E_lam [V "R"] $
            E_lam [V "X", V "Y"] $
              E_app (E_var $ V "R") [E_var $ V "X", E_var $ V "Y"] 
              `E_or`  
              E_ex (V "Z") 
                  (E_app (E_var $ V "R") [E_var $ V "X", E_var $ V "Z"]
                  `E_and` 
                  E_app (
                      E_app (E_predcon (C "closure") 1) [E_var $ V "R"]) 
                      [E_var $ V "Z", E_var $ V "Y"])
          
      )
  ] :
  []

