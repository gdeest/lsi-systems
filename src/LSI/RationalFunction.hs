{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSI.RationalFunction where

import LSI.System

import GHC.TypeLits
import Linear.V
import Data.Vector (fromList)

data NatProxy (d::Nat) = NatProxy

data RationalFunction (d :: Nat) c = Monomial c (V d Int)
                                   | Add (RationalFunction d c) (RationalFunction d c)
                                   | Mul (RationalFunction d c) (RationalFunction d c)
                                   | Div (RationalFunction d c) (RationalFunction d c)
                                   deriving (Show)

constant :: forall (d :: Nat) c. (KnownNat d) => c -> RationalFunction d c
constant k = Monomial k $ V . fromList . take d $ repeat 0
  where d = fromInteger . natVal $ proxy

        proxy :: NatProxy d
        proxy = NatProxy

one, zero :: (KnownNat d, Num c) => RationalFunction d c
one  = constant 1
zero = constant 0
