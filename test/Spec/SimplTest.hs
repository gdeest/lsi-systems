{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.SimplTest where

import Control.Applicative
import LSI.RationalFunction

import Data.Vector (fromList, toList)
import Data.Proxy
import GHC.TypeLits
import Test.QuickCheck
import Linear.V

instance (KnownNat d) => Arbitrary (V d Int) where
  arbitrary = V . fromList <$>
    vectorOf d arbitrary
    where d = fromInteger $ natVal (Proxy :: Proxy d)

instance (KnownNat d, Arbitrary c) => Arbitrary (RationalFunction d c) where
  arbitrary = sized sizedGen
    where sizedGen i = if i <= 1 then
                         Monomial <$> arbitrary <*> arbitrary
                       else
                         let h = i `div` 2
                             h' = i - h in
                         oneof
                            [ Mul <$> sizedGen h <*> sizedGen h'
                            , Div <$> sizedGen h <*> sizedGen h'
                            , Add <$> sizedGen h <*> sizedGen h'
                            ]

eval :: (Rational, Rational) -> RationalFunction 2 Rational -> Maybe Rational
eval (z1, z2) = eval'
  where eval' (Monomial c (V vs)) = do
            zv1 <- toPow z1 e1
            zv2 <- toPow z2 e2
            Just $ c * zv1 * zv2

          where zv1 = toPow z1 e1
                zv2 = toPow z2 e2
                [e1, e2] = toList vs
                toPow :: Rational -> Int -> Maybe Rational
                toPow z e = if e < 0 then
                              if z == 0 then Nothing
                              else (toPow z (-e)) >>= \r -> Just $ 1 / r
                            else Just $ foldr (*) 1 (take e $ repeat z)

        eval' (Mul r1 r2) = liftA2 (*) (eval' r1) (eval' r2)
        eval' (Div r1 r2) = do
          n <- eval' r1
          d <- eval' r2
          if d == 0 then Nothing else Just (n/d)
        eval' (Add r1 r2) = liftA2 (+) (eval' r1) (eval' r2)
