{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module LSI.RationalFunction where

import LSI.System

import GHC.TypeLits
import Linear.V

data NatProxy (d::Nat) = NatProxy

data RationalFunction (d :: Nat) c = One
                                   | Monomial c (V d Int)
                                   | Sum (RationalFunction d c) (RationalFunction d c)
                                   | Mul (RationalFunction d c) (RationalFunction d c)
                                   | Div (RationalFunction d c) (RationalFunction d c)
                                   deriving (Show)
