{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Sync (Sync, Private, fromPrivate, private, sync, unsafeSync, runSync) where

import Data.Kind
import Data.Singletons
import Data.Type.Equality
import Data.Void

{- | @Sync s msg m a@ represents shared knowledge of type 'a', originating
 from a computation in the monad 'm', that may be sent using a message of
 type 'msg a', where 's' is the side that we are actually on.
 (we are doing something like the ST trick here)

 When a pure value is introduced with 'pure', it is computed on both sides
 independently in the same way, and will therefore be identical.

 A value can be computed impurely on one side only using either 'sync' or
 'private'. The former will share the value with the other sides, while the
 latter will not. The 'Private' type represents values that are known only
 to one side.

 You can also use 'unsafeSync' to compute an impure value independently on all
 sides, and assert that it is the same without checking. This is necessary when
 e.g. computing a shared secret.

 Note that Sync will obviously fail if some pure difference is introduced
 before calling runSync: obviously the 'Sync' being called on both sides
 will not be the same. You should ensure that all values passed into 'Sync'
 from the outside are either pure, or trivially the same
 (in the latter case, it's best to make the assertion explicit with 'unsafeSync')
-}
newtype Sync (s :: k) (msg :: Type -> Type) m a = Sync (m a)
  deriving (Functor, Applicative, Monad)

-- | Secret value computed by side @s'@. Side 's' is the side we are on.
data Private s s' a where
  Have :: a -> Private s s a
  Haven't :: (s :~: s' -> Void) -> Private s s' a

-- | If we computed the secret value, we know it.
fromPrivate :: Private s s a -> a
fromPrivate (Have a) = a
fromPrivate (Haven't f) = absurd (f Refl)

instance Functor (Private s s') where
  fmap f (Have a) = Have (f a)
  fmap _ (Haven't r) = Haven't r

-- | If we are side @s'@, compute the value and share it.
sync :: SingI (msg a) => Sing s' -> (a -> msg a) -> (s ~ s' => m a) -> Sync s msg m a
sync sSide sMsg f = undefined

-- | If we are side @s'@, compute the value, but don't share it.
private :: Sing s' -> (s ~ s' => m a) -> Sync s msg m (Private s s' a)
private s f = undefined

{- | Compute the value independently on all sides, and assert that it is the
 same without checking
-}
unsafeSync :: (Sing s -> m a) -> Sync s msg m a
unsafeSync f = undefined

{-
 Carry out a Sync computation, after specifying the side we are on and how to
 receive/send messages.
-}
runSync :: Sing side -> (forall x. Sing (msg x) -> m x) -> (forall x. msg x -> m ()) -> (forall s. Sync s msg m a) -> m a
runSync s recv send snc = undefined
