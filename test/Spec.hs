{-# LANGUAGE DataKinds #-}

import Debug.Trace

import Test.QuickCheck
import Spec.SimplTest

import LSI.RationalFunction

prop_simpl :: RationalFunction 2 Rational -> (Rational, Rational) -> Bool
prop_simpl r zs = let r' = simplify r in
  eval zs r' == eval zs r

main :: IO ()
main = do
  let args = stdArgs { maxSize = 40 }
  quickCheckWith args prop_simpl
