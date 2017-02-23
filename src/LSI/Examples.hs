{-# LANGUAGE DataKinds #-}

import LSI

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
