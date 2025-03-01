{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE EmptyCase #-}
module Main (main) where

import Crypto.PubKey.DH
import Control.Monad.Sync
import Control.Concurrent.Async
import Control.Monad.Trans.Cont
import Data.Singletons
import Control.Concurrent.STM
import Data.Kind
import Control.Monad.Trans.Class
import Control.Exception
import Data.Singletons.Decide


data Side = A | B

data SSide (s :: Side) where
  SA :: SSide 'A
  SB :: SSide 'B

type instance Sing = SSide

instance SingI 'A where
  sing = SA

instance SingI 'B where
  sing = SB
instance SDecide Side where
  (%~) SA SA = Proved Refl
  (%~) SB SB = Proved Refl
  (%~) SA SB = Disproved $ \case {}
  (%~) SB SA = Disproved $ \case {}

data Msg a where
  PubKey :: PublicNumber -> Msg PublicNumber
  DhParams :: Params -> Msg Params

data SMsg (a :: Type) where
  SPubKey :: SMsg (Msg PublicNumber)
  SDhParams :: SMsg (Msg Params)

type instance Sing = SMsg

instance SingI (Msg PublicNumber) where
  sing = SPubKey

instance SingI (Msg Params) where
  sing = SDhParams


data Some (f :: k -> Type) where
  Some :: f a -> Some f


data CommsHandle = CommsHandle
  { msgsToA :: !(TQueue (Some Msg))
  , msgsToB :: !(TQueue (Some Msg))
  }


main :: IO ()
main = evalContT $ do
  msgsToA <- lift newTQueueIO
  msgsToB <- lift newTQueueIO

  let commsHandle = CommsHandle{msgsToA, msgsToB}

  b <- ContT . withAsync $ mainB commsHandle

  lift $ link b

  sSecretA <- lift $ mainA commsHandle
  sSecretB <- lift $ wait b

  if sSecretA == sSecretB
    then lift $ putStrLn "Shared secret is the same"
    else lift $ putStrLn "(!!!) Shared secrets are different"


mainA :: CommsHandle -> IO SharedKey
mainA CommsHandle{msgsToA, msgsToB} = runSync SA (receiveMessage msgsToA) (sendMessage msgsToB) sharedSecret

mainB :: CommsHandle -> IO SharedKey
mainB CommsHandle{msgsToA, msgsToB} = runSync SB (receiveMessage msgsToB) (sendMessage msgsToA) sharedSecret


receiveMessage :: TQueue (Some Msg) -> Sing (Msg a) -> IO a
receiveMessage q s = do
  x <- atomically $ readTQueue q
  case (s, x) of
    (SPubKey, Some (PubKey pn)) -> pure pn
    (SDhParams, Some (DhParams p)) -> pure p
    _ -> throwIO $ userError "Unexpected message"

sendMessage :: TQueue (Some Msg) -> Msg a -> IO ()
sendMessage q m = atomically $ writeTQueue q (Some m)


-- | Exchange a DH shared secret between sides A and B
sharedSecret :: Sync (s :: Side) Msg IO SharedKey
sharedSecret = do
  -- A decides on the parameters
  params <- sync SA DhParams $ do
    putStrLn "A: Generating DH parameters..."
    p <- generateParams 128 2
    putStrLn $ "A: Generated parameters: " ++ show p
    pure p
  
  _ <- private SB (Proxy :: Proxy ()) $ do
    putStrLn $ "B: Received DH parameters, they are: " ++ show params

  -- A and B generate their keypairs
  privA <- private SA (Proxy :: Proxy PrivateNumber) $ do
    putStrLn "A: Generating private key..."
    generatePrivate params
  privB <- private SB (Proxy :: Proxy PrivateNumber) $ do
    putStrLn "B: Generating private key..."
    generatePrivate params

  -- A and B share their public keys
  pubA <- sync SA PubKey $ do
    putStrLn "A: Sending public key..."
    pure (calculatePublic params (fromPrivate privA))
  pubB <- sync SB PubKey $ do
    putStrLn "B: Sending public key..."
    pure (calculatePublic params (fromPrivate privB))

  -- A and B compute the shared secret independently
  unsafeSync $ \case
    SA -> pure $ getShared params (fromPrivate privA) pubB
    SB -> pure $ getShared params (fromPrivate privB) pubA

