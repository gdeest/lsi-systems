{-# LANGUAGE DataKinds #-}
module LSI.Examples where

import LSI

-- Simple Mul

mul :: System 1 String Float
mul = y
  where x = input "x"
        y = 0.5 *: x @: (-1)

-- FIR (non-recursive) one-dimensional filter.
fir :: System 1 String Float
fir = y
  where x = input "x"
        y =  x
          +: (-0.75) *: x@:(-1)

-- Simple one-dimensional IIR filter
iir :: System 1 String Float
iir = y
  where x = input "x"
        y =  x
          -- Non-recursive part
          +: 0.5   *: x@:(-1)
          +: 0.3   *: x@:(-2)

          -- Recursive part
          +: (-1)  *: y@:(-1)
          +: 0.123 *: y@:(-2)

-- 2D Sobel Filter (x-gradient computation)
sobel_x :: System 2 String Float
sobel_x = y
  where x = input "I"
        y = coeffs `dot` xs

        coeffs = [ -1, 0, 1
                 , -2, 0, 2
                 , -1, 0, 1 ]

        delays = [ (-1,-1), ( 0,-1), ( 1,-1)
                 , (-1, 0), ( 0, 0), ( 1, 0)
                 , (-1, 1), ( 0, 1), ( 1, 1) ]

        xs = (repeat x) `off` delays
