{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module LSI.FrequencyResponse where

import Data.Complex
import Data.Vector as V hiding (map, zip, foldr)
import Linear.V as V


import LSI.RationalFunction

computeFR :: RealFloat f
          => RationalFunction d f
          -> V d f
          -> Complex f
computeFR rf = (computeFR' rf) . V.toList .V.toVector
  where computeFR' f =
          case f of
            Monomial c es ->
              let fs = map toInteger . V.toList . V.toVector $ es in
                \vs -> {-# SCC monom #-}
                  let factors = map iexp $ zip zs fs
                      iexp (z, e) | e < 0 = 1/(z^(-e))
                      iexp (z, e) = z^e
                      zs = map (\w -> mkPolar 1.0 w) vs in
                    (c :+ 0.0) * (foldr (*) 1 factors)

            Add rf1 rf2 ->
              let fr1 = computeFR' rf1
                  fr2 = computeFR' rf2 in
                \vs -> (fr1 vs) + (fr2 vs)

            Mul rf1 rf2 ->
              let fr1 = computeFR' rf1
                  fr2 = computeFR' rf2 in
                \vs -> (fr1 vs) * (fr2 vs)

            Div rf1 rf2 ->
              let fr1 = computeFR' rf1
                  fr2 = computeFR' rf2 in
                \vs -> (fr1 vs) / (fr2 vs)
