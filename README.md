# sync-monad

A very basic, unintentional and independent reinvention of
_functional choreographic programming_. Use
[HasChor](https://hackage.haskell.org/package/HasChor) instead, unless you
really need to use GHC 8.10.7.

## Example: Diffie-Hellman key exchange

See `examples/DiffieHellman.hs` for a fully-implemented example that is also
more likely to be kept up to date.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}

-- crypton
import Crypto.PubKey.DH
-- singletons
import Data.Singletons
-- sync-monad
import Control.Monad.Sync

data Side = A | B

data Msg (s :: Side) a where
    PubKey :: Msg s PublicNumber
    DhParams :: Msg 'A Params

data SSide = SA | SB

-- (insert SingI, SDecide boilerplate here)

mainA :: IO SharedKey
mainA = runSyncSameMonad itpA sharedSecret

mainB :: IO SharedKey
mainB = runSyncSameMonad itpB sharedSecret

sharedSecret :: Sync (s :: Side) Msg IO SharedKey
sharedSecret = do
  -- A decides on the parameters
  params <- sync SA DhParams (generateParams 128 2)
  
  -- A and B generate their keypairs
  privA <- private SA (Proxy :: Proxy PrivateNumber) (generatePrivate params)
  privB <- private SB (Proxy :: Proxy PrivateNumber) (generatePrivate params)

  -- A and B share their public keys
  pubA <- sync SA PubKey $ pure (calculatePublic params (fromPrivate privA))
  pubB <- sync SB PubKey $ pure (calculatePublic params (fromPrivate privB))

  -- A and B compute the shared secret independently
  unsafeSync $ \case
    SA -> pure $ getShared params (fromPrivate privA) pubB
    SB -> pure $ getShared params (fromPrivate privB) pubA

itpA :: Interpreter 'A Msg IO
itpA = Interpreter
  { side = SA
  , recv = error "unimplemented"
  , send = error "unimplemented"
  }

itpB :: Interpreter 'B Msg IO
itpB = Interpreter
  { side = SB
  , recv = error "unimplemented"
  , send = error "unimplemented"
  }
```
