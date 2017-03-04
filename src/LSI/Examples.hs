{-# LANGUAGE DataKinds #-}
module LSI.Examples where

import LSI

-- Simple scaling of a signal:
-- y(t) = 0.5 x(t)
mul :: System 1 String Float
mul = y
  where x = input "x"
        y = 0.5 *: x

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

-- Second-order Chebyshev Type-1 Filter
cheby1 :: System 1 String Double
cheby1 = y
  where x = input "x"
        y =  0.94717   *: x
          +: (-0.189434) *: x @:(-1)
          +: (0.094717)  *: x @:(-2)
          +: 0.53756     *: y @:(-1)
          +: 0.0973565   *: y @:(-2)

-- b =

--    0.094717  -0.189434   0.094717

-- a =

--    1.00000   0.53756   0.73565


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


-- 2D Deriche Filter (generic pattern).
deriche :: c -> c -> c -> c -- a1 .. a4
        -> c -> c -> c -> c -- a5 .. a8
        -> c -> c           -- b1 .. b2
        -> c -> c           -- c1 .. c2
        -> System 2 String c

deriche a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 c1 c2 = theta2
  where
    -- Graph nodes
    x = input "x"

    y1 =
         a1 *: x
      +: a2 *: x  @:(0,-1)
      +: b1 *: y1 @:(0,-1)
      +: b2 *: y1 @:(0,-2)

    y2 =
         a3 *: x  @: (0,1)
      +: a4 *: x  @: (0,2)
      +: b1 *: y2 @: (0,1)
      +: b2 *: y2 @: (0,2)

    y3 =
         a5 *: theta1
      +: a6 *: theta1  @:(-1,0)
      +: b1 *: y3      @:(-1,0)
      +: b2 *: y3      @:(-2,0)

    y4 =
         a7 *: theta1  @:(1,0)
      +: a8 *: theta1  @:(2,0)
      +: b1 *: y4      @:(1,0)
      +: b2 *: y4      @:(2,0)

    theta1 =
         c1 *: y1
      +: c1 *: y2

    theta2 =
         c2 *: y3
      +: c2 *: y4


deriche_ydiff :: Floating a => a -> System 2 String a
deriche_ydiff alpha = deriche a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 c1 c2
  where
    -- Common expressions
    ea  = exp(-alpha)
    e2a = exp(-2*alpha)
    k = (1-ea)*(1-ea) / (1 + 2*alpha*ea - e2a)

    -- Coefficients
    a1 = k
    a2 = k*ea*(alpha-1)
    a3 = k*ea*(alpha+1)
    a4 = -k*e2a

    a5 = 0
    a6 = 1
    a7 = -1
    a8 = 0

    b1 = 2*ea
    b2 = -e2a

    c1 = 1
    c2 = -(1-ea)*(1-ea)

deriche_smooth :: Floating a => a -> System 2 String a
deriche_smooth alpha = deriche a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 c1 c2
  where
    -- Common expressions
    ea  = exp(-alpha)
    e2a = exp(-2*alpha)
    k = (1-ea)*(1-ea) / (1 + 2*alpha*ea - e2a)

    -- Coefficients
    a1 = k
    a2 = k*ea*(alpha-1)
    a3 = k*ea*(alpha+1)
    a4 = -k*e2a

    a5 = a1
    a6 = a2
    a7 = a3
    a8 = a4

    b1 = 2*ea
    b2 = -e2a

    c1 = 1
    c2 = 1
