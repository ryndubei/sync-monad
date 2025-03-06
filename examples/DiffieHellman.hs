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
import Data.Void


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

data Msg s a where
  PubKey :: PublicNumber -> Msg s PublicNumber
  DhParams :: Params -> Msg s Params

data SMsg (a :: Type) where
  SPubKey :: Sing s -> SMsg (Msg s PublicNumber)
  SDhParams :: Sing s -> SMsg (Msg s Params)

type instance Sing = SMsg

instance SingI s => SingI (Msg s PublicNumber) where
  sing = SPubKey sing

instance SingI s => SingI (Msg s Params) where
  sing = SDhParams sing


data Some (f :: k -> Type) where
  Some :: f a -> Some f


data CommsHandle = CommsHandle
  { msgsToA :: !(TQueue (Some (Msg 'B)))
  , msgsToB :: !(TQueue (Some (Msg 'A)))
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

aInterpreter :: CommsHandle -> Interpreter 'A Msg IO
aInterpreter CommsHandle{msgsToA, msgsToB} = Interpreter
  { side = SA
  , send = sendMessage msgsToB
  , recv = \ _ SB -> receiveMessage msgsToA
  }

bInterpreter :: CommsHandle -> Interpreter 'B Msg IO
bInterpreter CommsHandle{msgsToA, msgsToB} = Interpreter
  { side = SB
  , send = sendMessage msgsToA
  , recv = \r -> \case
      SA -> receiveMessage msgsToB
      SB -> absurd $ r Refl
  }

mainA :: CommsHandle -> IO SharedKey
mainA h = runSyncSameMonad (aInterpreter h) sharedSecret

mainB :: CommsHandle -> IO SharedKey
mainB h = runSyncSameMonad (bInterpreter h) sharedSecret

receiveMessage :: TQueue (Some (Msg sender)) -> Sing (Msg sender a) -> IO a
receiveMessage q s = do
  x <- atomically $ readTQueue q
  case (s, x) of
    (SPubKey _, Some (PubKey pn)) -> pure pn
    (SDhParams _, Some (DhParams p)) -> pure p
    _ -> throwIO $ userError "Unexpected message"

sendMessage :: TQueue (Some (Msg side)) -> Msg side a -> IO ()
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

