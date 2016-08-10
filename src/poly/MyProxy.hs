
module MyProxy (
  module MyProxy,
  module Control.Monad.Coroutine,
  module Control.Monad.Coroutine.SuspensionFunctors)
where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans

{-
  Coroutine playground
  consult
  https://hackage.haskell.org/package/monad-coroutine-0.9.0.1
  https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/coroutines-for-streaming/part-3-stacking-interfaces
-}

type Producing o i = Coroutine (Request o i)
type Consuming r m i o = i -> Producing o i m r

($$) :: Monad m => Producing i o m r -> Consuming r m i o -> m r
producer $$ consumer = resume producer >>= either f return
  where f (Request x g) = consumer x $$ g

foreverK :: Monad m => (a -> m a) -> a -> m b
foreverK m a = m a >>= foreverK m


consume :: Monad m => (a -> b) -> Consuming r m a b
consume f = foreverK (\x -> request (f x))

fuse :: Monad m => Consuming r m a b -> Consuming r m b c -> Consuming r m a c
fuse c1 c2 = \a -> lift (resume (c1 a)) >>= either f return
  where f (Request b g) = lift (resume (c2 b)) >>= either (h g) return
        h g1 (Request c g2) = suspend $ Request c (fuse g1 g2)


echo :: Monad m => Consuming r m a a
echo = foreverK request

-- Proxies

type Proxy r m i1 o1 i2 o2 =  Consuming r (Producing i2 o2 m) i1 o1

type Pipe r m a b = Proxy r m a () b ()

pipe :: Monad m => (a -> b) -> Pipe r m a b
pipe f = foreverK $ \x -> respond (f x) >> request ()

idPipe :: Monad m => Pipe r m a a
idPipe = pipe id

--respond :: a -> Proxy a m a b a b
respond x = lift (request x)

idProxy :: Monad m => Proxy r m a b a b
idProxy = foreverK (\x -> respond x >>= request)

insert0 :: (Monad m, Functor s) => m a -> Coroutine s m a
insert0 = lift
insert1 :: (Monad m, Functor s, Functor s1) => Coroutine s1 m a -> Coroutine s1 (Coroutine s m) a
insert1 = mapMonad insert0
insert2 :: (Monad m, Functor s, Functor s0, Functor s1) => Coroutine s0 (Coroutine s1 m) a -> Coroutine s0 (Coroutine s1 (Coroutine s m)) a
insert2 = mapMonad insert1

commute ::  forall a b c d m r. Monad m => Producing a b (Producing c d m) r -> Producing c d (Producing a b m) r
commute p = p' $$ funnel
  where --p' :: Producing a b (Producing c d (Producing a b m)) r
        p' = insert2 p
        --funnel :: Consuming r (Producing c d (Producing a b m)) a b
        funnel a = insert1 (idProxy a)

($=) :: Monad m => Producing a b m r -> Proxy r m a b c d -> Producing c d m r
p $= proxy = insert1 p $$ proxy

(=$) :: Monad m => Proxy r m a b c d -> Consuming r m c d -> Consuming r m a b
proxy =$ c = \a -> p a $$ (insert1 . c)
  where p a = commute (proxy a)

(=$=) :: Monad m => Proxy r m a a' b b' -> Proxy r m b b' c c' -> Proxy r m a a' c c'
proxy1 =$= proxy2 = \a -> p a $$ c
  where p a = insert2 $ commute $ proxy1 a
        c a = insert1 $ proxy2 a
