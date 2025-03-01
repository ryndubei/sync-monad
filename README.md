# sync-monad

A very basic, unintentional and independent reinvention of
_functional choreographic programming_. Use
[HasChor](https://hackage.haskell.org/package/HasChor)[^1] instead, unless you
really need to use GHC 8.10.7.

## Example: Diffie-Hellman key exchange

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
-- singletons-th
import Data.Singletons.TH
-- sync-monad
import Control.Monad.Sync

data Side = A | B

data Msg a where
    PubKey :: PublicNumber -> Msg PublicNumber
    DhParams :: Params -> Msg Params

$(genSingletons [''Side, ''Msg])

$(singDecideInstance ''Side)

mainA :: IO SharedKey
mainA = runSync SA receiveMessageB2A sendMessageA2B sharedSecret

mainB :: IO SharedKey
mainB = runSync SB receiveMessageA2B sendMessageB2A sharedSecret

sharedSecret :: Sync (s :: Side) Msg IO SharedKey
sharedSecret = do
  -- A decides on the parameters
  params <- sync SA DhParams (generateParams 128 2)
  
  -- A and B generate their keypairs (the type signatures are mandatory,)
  (privA :: Private s 'A PrivateNumber) <- private SA (generatePrivate params)
  (privB :: Private s 'B PrivateNumber) <- private SB (generatePrivate params)

  -- A and B share their public keys
  pubA <- sync SA PubKey $ pure (calculatePublic params (fromPrivate privA))
  pubB <- sync SB PubKey $ pure (calculatePublic params (fromPrivate privB))

  -- A and B compute the shared secret independently
  unsafeSync $ \case
    SA -> pure $ getShared params (fromPrivate privA) pubB
    SB -> pure $ getShared params (fromPrivate privB) pubA

receiveMessageA2B, receiveMessageB2A :: forall x. SMsg x -> IO x
receiveMessageA2B = error "unimplemented"
receiveMessageB2A = error "unimplemented"

sendMessageA2B, sendMessageB2A :: forall x. Msg x -> IO ()
sendMessageA2B = error "unimplemented"
sendMessageB2A = error "unimplemented"
```
