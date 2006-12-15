module TcUtils where

import HOPL

type TyVar = Int

data TcTyp =
    Tcvar TyVar
  | Tcground
  | Tcbool
  | Tcfunc [TcTyp] TcTyp

type TcSub = [(TyVar, TcTyp)]
type TEnv  = [(Var, TcTyp)]

type TcState = (TyVar, TEnv, TcSub)

newtype TcM a = TcM (TcState -> (TcState, Either ErrMsg a))

instance Monad TcM where
   TcM m >>= f = TcM $ \s -> 
       let (s', a) = m s 
           TcM m'  = f a
       in  m' s'
   return a = TcM (\s -> (s, Left a))
   fail err = TcM (\s -> (s, Right err)) 

newTyVar :: TcM TcTyp
newTyVar = TcM (\(n,e,s) -> ((n+1, e, s),Tcvar n))

getEnv :: TcM TEnv
getEnv = TcM (\env -> (env, Left env))
 
extendEnv :: Var -> TcTyp -> TcM a -> TcM a 
extendEnv v t (TcM m) =
    let extend (uniq, gamma, sigma) = (uniq, (v,t):gamma, sigma) 
    in  TcM (\env -> m (extend env))

lookupVar :: Var -> TcM (Maybe TcTyp)
lookupVar v = do
   env <- getEnv
   return (lookup v)

check :: Bool -> ErrMsg -> TcM ()
check False err = fail err
check True  _   = return ()
