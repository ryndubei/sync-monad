{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Monad.Sync (Interpreter(..), Sync, Private, fromPrivate, private, sync, unsafeSync, runSync, runSyncSameMonad) where

import Data.Kind
import Data.Singletons
import Data.Type.Equality
import Data.Void
import Control.Monad.Free
import Data.Singletons.Decide

{- | @Sync s msg m a@ represents shared knowledge of type 'a', originating
 from a computation in the monad 'm', that may be sent using a message of
 type 'msg s a', where 's' is the side that we are actually on.
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
newtype Sync (s :: k) (msg :: k -> Type -> Type) m a = Sync (Free (SyncAction s msg m) a)
  deriving (Functor, Applicative, Monad)
  -- note that we aren't using FreeT, because this isn't a monad transformer!

data SyncAction (s :: k) msg m next where
  CallSync :: Sing (s' :: k) -> Sing (msg s' a) -> (a -> msg s' a) -> (s :~: s' -> m a) -> (a -> next) -> SyncAction s msg m next
  CallPrivate :: Sing (s' :: k) -> (s :~: s' -> m a) -> (Private s s' a -> next) -> SyncAction s msg m next
  CallUnsafeSync :: (Sing s -> m a) -> (a -> next) -> SyncAction s msg m next

deriving instance Functor (SyncAction k msg m)

-- | Secret value computed by side @s'@. Side 's' is the side we are on.
newtype Private s s' a = Private (s :~: s' -> a) deriving Functor

-- | If we computed the secret value, we know it.
fromPrivate :: Private s s a -> a
fromPrivate (Private f) = f Refl

-- | If we are side @s'@, compute the value and share it.
sync :: SingI (msg s' a) => Sing s' -> (a -> msg s' a) -> (s ~ s' => m a) -> Sync s msg m a
sync sSide (mkMsg :: (a -> msg a)) a = Sync . liftF $ CallSync sSide (sing :: Sing (msg a)) mkMsg (\Refl -> a) id

-- | If we are side @s'@, compute the value, but don't share it.
private :: Sing s' -> Proxy a -> (s ~ s' => m a) -> Sync s msg m (Private s s' a)
private sSide _ a = Sync . liftF $ CallPrivate sSide (\Refl -> a) id

{- | Compute the value independently on all sides, and assert that it is the
 same without checking
-}
unsafeSync :: (Sing s -> m a) -> Sync s msg m a
unsafeSync f = Sync . liftF $ CallUnsafeSync f id

data Interpreter side msg m = Interpreter
  { side :: !(Sing side)
  , send :: forall x. msg side x -> m ()
  -- Sing (msg sender x) should already have 'Sing sender', but passing in an
  -- extra 'Sing sender' reduces the number of pattern matches necessary when
  -- defining 'recv'
  , recv :: forall x sender. (sender :~: side -> Void) -> Sing sender -> Sing (msg sender x) -> m x
  }

{-
 Carry out a Sync computation, after specifying the side we are on and how to
 receive/send messages.

 Note that we can use a different monad on each side.
-}
runSync :: (Monad m, SDecide k, m ~ m' side) => Interpreter (side :: k) msg m -> (forall (s :: k). Sync s msg (m' s) a) -> m a
runSync = runSync'

{-
 Like 'runSync', but using the same monad on all sides.
-}
runSyncSameMonad :: (Monad m, SDecide k) => Interpreter (side :: k) msg m -> (forall (s :: k). Sync s msg m a) -> m a
runSyncSameMonad = runSync'

runSync' :: (Monad m, SDecide k) => Interpreter (side :: k) msg m -> Sync side msg m a -> m a
runSync' _ (Sync (Pure a)) = pure a
runSync' itp@Interpreter{side, send, recv} (Sync (Free a)) = case a of
  CallSync s' sMsg mkMsg f next -> case side %~ s' of
    Proved Refl -> do
      msg <- f Refl
      send (mkMsg msg)
      runSync' itp (Sync $ next msg)
    Disproved r -> do
      msg <- recv (\Refl -> r Refl) s' sMsg -- if we pass 'r' directly the typechecker dies
      runSync' itp (Sync $ next msg)
  CallPrivate s' f next -> case side %~ s' of
    Proved Refl -> f Refl >>= \x -> runSync' itp . Sync . next $ Private (\Refl -> x)
    Disproved g -> runSync' itp (Sync $ next (Private (absurd . g)))
  CallUnsafeSync f next -> f side >>= runSync' itp . Sync . next
