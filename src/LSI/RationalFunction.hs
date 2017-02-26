{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSI.RationalFunction where


import GHC.TypeLits
import Linear.V
import Data.List (intersperse)
import Data.Vector (fromList)

import Data.Proxy
import Data.Text (Text)
import Debug.Trace

import qualified Data.Text as T
import qualified Data.Vector as V


data RationalFunction (d :: Nat) c = Monomial c (V d Int)
                                   | Add (RationalFunction d c) (RationalFunction d c)
                                   | Mul (RationalFunction d c) (RationalFunction d c)
                                   | Div (RationalFunction d c) (RationalFunction d c)
                                   deriving (Show)

constant :: forall d c. (KnownNat d) => c -> RationalFunction d c
constant k = Monomial k $ V . fromList . take d $ repeat 0
  where d = fromInteger $ natVal (Proxy :: Proxy d)

one, zero :: (KnownNat d, Num c) => RationalFunction d c
one  = constant 1
zero = constant 0

equals :: (Eq c, Num c) => c -> RationalFunction d c -> Bool
equals c (Monomial k v) | all ((==) 0) v && c == k = True
equals _ _ = False

isOne, isZero :: (Eq c, Num c) => RationalFunction d c -> Bool
isOne  = equals 1
isZero = equals 0

-- | Simplify a rational function to be either:
-- - A right-unbalanced polynomial. E.g.: Add(m1, Add(m2, Add (m3, m4)))
--   such that coefficients appear only once, in increasing lexicographic order.
-- - A division of such monomial sums.
--
-- In case of a division of polynomials, no attempt is made to perform polynomial division.

simplify :: (KnownNat d, Eq c, Num c, Show c) => RationalFunction d c -> RationalFunction d c
simplify rat = case rat of
  -- Base case. Return self.
  Monomial _ _ -> rat

  -- Addition.
  Add r1 r2 -> case (simplify r1, simplify r2) of
    -- Shortcut cases
    (r1, r2) | isZero r1 -> r2
    (r1, r2) | isZero r2 -> r1

    -- One of the expressions is a division.
    (Div n1 d1, Div n2 d2) -> Div (polyAdd (polyMul n1 d2) (polyMul n2 d1)) (polyMul d1 d2)
    (r1, Div n2 d2)        -> Div (polyAdd (polyMul r1 d2) n2) d2
    (Div n1 d1, r2)        -> Div (polyAdd (polyMul r2 d1) n1) d1

    -- Both expressions are polynomials.
    (r1, r2)               -> polyAdd r1 r2

  -- Multiplication.
  Mul r1 r2 -> case (simplify r1, simplify r2) of
    -- Shortcut cases.
    (r1, r2) | isZero r1 || isZero r1 -> zero
    (r1, r2) | isOne  r1 -> r2
    (r1, r2) | isOne  r2 -> r1

    -- One of the expressions is a division.
    (Div n1 d1, Div n2 d2) -> Div (polyMul n1 n2) (polyMul d1 d2)
    (r1, Div n2 d2)        -> Div (polyMul r1 n2) d2
    (Div n1 d1, r2)        -> Div (polyMul n1 r2) d1

    -- Other cases.
    (r1, r2)               -> polyMul r1 r2

  -- Division.
  Div r1 r2 -> case (simplify r1, simplify r2) of
    -- If r2 is unit, only return the numerator.
    (r1, r2) | isOne r2 -> r1

    -- One of the expressions is a division.
    (Div n1 d1, Div n2 d2) -> Div (polyMul n1 d2) (polyMul n2 d1)
    (r1, Div n2 d2)        -> Div (polyMul r1 d2) n2
    (Div n1 d1, r2)        -> Div n1 (polyMul d1 r2)

    -- Both expressions are polynomials. Nothing to do.
    (r1, r2) -> Div r1 r2

polyMul, polyAdd :: (KnownNat d, Show c, Eq c, Num c) => RationalFunction d c -> RationalFunction d c -> RationalFunction d c

-- Naive polynomial multiplication.
polyMul p1 p2 | isZero p1 || isZero p2 = zero
polyMul p1 p2 = case p1 of
  Monomial k1 es1 -> case p2 of
    Add (Monomial k2 es2) rst ->
      polyAdd (Monomial (k1*k2) (es1+es2)) (polyMul p1 rst)
    -- Base case.
    Monomial k2 es2 -> Monomial (k1*k2) (es1+es2)
  Add m1 rst -> polyAdd (polyMul m1 p2) (polyMul rst p2)

-- Naive polynomial addition.
  --
-- Maintains the following invariant: polynomials are in normal form, ie., the
-- exponents are increasing (lexicographic) order, each exponent appears only
-- once in the list and there is no zero coefficient.
-- polyAdd p1 p2 | isZero p1 = p2
-- polyAdd p2 p1 | isZero p2 = p1
polyAdd p1 p2 = case (p1, p2) of
  (Monomial k1 es1, Monomial k2 es2) ->
    if es1 < es2 then Add p1 p2
    else
      if es1 > es2 then Add p2 p1
      else Monomial (k1+k2) es1

  (Monomial k1 es1, Add h@(Monomial k2 es2) rst) ->
    if es1 < es2 then Add p1 p2
    else
      if es1 > es2 then case (polyAdd p1 rst) of
        Monomial c _ | c == 0 -> h
        p -> Add h p
      else let c = k1+k2 in
        if c == 0 then rst
        else Add (Monomial c es2) rst

  (Add m rst, _) -> polyAdd m (polyAdd rst p2)

-- Ugly-print the rational function as a Matlab / Octave anonymous function.
toMatlabFunction :: forall d c. (KnownNat d, Ord c, Num c, Show c) => RationalFunction d c -> T.Text
toMatlabFunction r = T.concat [ functionHead
                              , " "
                              , showBody r
                              ]

  where functionHead = let d = natVal (Proxy :: Proxy d) in
                         T.concat [ "@("
                                  , T.concat .
                                    intersperse "," .
                                    map (\i -> T.concat ["z", T.pack (show i)]) $
                                    [1..d]
                                  , ")"
                                  ]

        showBody r = case r of
          Monomial c exps -> T.concat [ showConstant $ c
                                      , ".*"
                                      , T.concat .
                                        intersperse ".*" .
                                        map (\(i,v) -> T.concat ["z", T.pack . show $ i, ".^", showConstant v]) $
                                        pairs
                                      ]
            where pairs = zip [1..] (V.toList . toVector $ exps)
                  showConstant d = let txt = T.pack (show d) in
                                if d >= 0 then txt
                                else T.concat ["(", txt, ")"]
          Add r1 r2 -> wrap ".+" r1 r2
          Mul r1 r2 -> wrap ".*" r1 r2
          Div r1 r2 -> wrap "./" r1 r2

        wrap operator r1 r2 = T.concat [ "(", showBody r1, ")"
                                       , operator
                                       , "(", showBody r2, ")"
                                       ]
