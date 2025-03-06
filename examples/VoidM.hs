{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
module Main (main) where

import Control.Monad.Sync
import Control.Monad.Trans.Reader
import Data.Void
import Data.Type.Bool
import Data.Type.Equality
import Data.Singletons
import Data.Kind
import Data.Singletons.Decide

type VoidM = Reader Void

neverHappens :: Reader Void a
neverHappens = do
  v <- ask
  absurd v

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
  SA %~ SA = Proved Refl
  SB %~ SB = Proved Refl
  SA %~ SB = Disproved $ \case {}
  SB %~ SA = Disproved $ \case {}

data Msg s a where
  Msg :: Int -> Msg s Int

data SMsg (a :: Type) where
  SMsg :: Sing s -> SMsg (Msg s Int)

type instance Sing = SMsg
instance SingI s => SingI (Msg s Int) where
  sing = SMsg sing

newtype AM s a = AM { unam :: If (s == 'A) (IO a) (VoidM a) }

-- GHC isn't smart enough to figure out that AM 'A ~ IO by itself when
-- doing standalone deriving, so have to do this
deriving via IO instance Functor (AM 'A)
deriving via IO instance Applicative (AM 'A)
deriving via IO instance Monad (AM 'A)
deriving via (Reader Void) instance Functor (AM 'B)

chor :: Sync s Msg (AM s) Int
chor = do
  msg1 <- sync SA Msg $ do
    AM (putStrLn "Computing our own message...")
    pure 42
  msg2 <- sync SB Msg (AM neverHappens)
  pure (msg1 + msg2)

itp :: Interpreter 'A Msg (AM 'A)
itp = Interpreter
  { side = SA
  , send = \case
      Msg i -> AM (putStrLn $ "Pretending to send message: " ++ show i)
  , recv = \ _ SB -> \case
      SMsg _ -> do
        AM (putStrLn "Pretending to receive message...")
        pure 27
  }

main :: IO ()
main = do
  x <- unam $ runSync itp chor
  print x

