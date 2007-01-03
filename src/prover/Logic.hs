{-# OPTIONS -fglasgow-exts #-}

module Logic where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

class MonadPlus m => MonadLogic m where
    msplit :: m a -> m (Maybe (a, m a))
    interleave :: m a -> m a -> m a
    interleave sg1 sg2 = do
        r <- msplit sg1
        case r of
            Nothing -> sg2
            Just (a,b) ->
                (return a) `mplus` (interleave sg2 b)
    (>>-) :: m a -> (a -> m b) -> m b
    sg >>- g = do
        r <- msplit sg
        case r of
            Nothing -> mzero
            Just (a,b) -> interleave (g a) (b >>- g)
    ifte  :: m a -> (a -> m b) -> m b -> m b
    ifte t th el = do
        r <- msplit t
        case r of
            Nothing -> el
            Just (a,b) -> (th a) `mplus` (b >>= th)
    once  :: m a -> m a
    once m = do
        r <- msplit m
        case r of
            Nothing -> mzero
            Just (a,_) -> return a

newtype LogicT m a  = SFKT (forall ans. SK (m ans) a -> FK (m ans) -> m ans)
unSFKT (SFKT a) = a

type FK ans = ans
type SK ans a = a -> FK ans -> ans

instance Monad m => Monad (LogicT m) where
    return e = SFKT (\sk fk -> sk e fk)
    m >>= f  =
      SFKT (\sk fk ->
           unSFKT m (\a fk' -> unSFKT (f a) sk fk')
             fk)
    fail s   = SFKT (\_ fk -> fk)

instance Monad m => MonadPlus (LogicT m) where
    mzero = SFKT (\_ fk -> fk)
    m1 `mplus` m2 = SFKT (\sk fk -> unSFKT m1 sk (unSFKT m2 sk fk))


instance MonadTrans LogicT where
    lift m = SFKT (\sk fk -> m >>= (\a -> sk a fk))

instance MonadIO m => MonadIO (LogicT m) where
    liftIO = lift . liftIO

instance Monad m => MonadLogic (LogicT m) where
    msplit m = lift $ unSFKT m ssk (return Nothing)
        where ssk a fk = return $ Just (a, (lift fk >>= reflect))

reflect r = case r of
                Nothing -> mzero
                Just (a,tmr) -> return a `mplus` tmr

instance MonadLogic m => MonadLogic (StateT s m) where
    msplit (StateT m) =
        StateT $ \s -> 
            msplit (m s) >>= \r ->
                case r of 
                    Nothing -> return (Nothing, s)
                    Just ((a, s'), m') ->
                        return (Just (a, StateT $ \s -> m' ), s')


runL :: (Monad m) => Maybe Int -> LogicT m a -> m [a]
runL Nothing (SFKT m) = m (\a fk -> fk >>= (return . (a:))) (return [])
runL (Just n) (SFKT m) | n <=0 = return []
runL (Just 1) (SFKT m) = m (\a fk -> return [a]) (return [])
runL (Just n) m = unSFKT (msplit m) runM' (return [])
    where runM' Nothing _ = return []
          runM' (Just (a,m')) _ = runL (Just (n-1)) m' >>= (return . (a:))

observe m = unSFKT m (\a fk -> return a) (fail "no answer")