# Introduction

Linear, shift-invariant (LSI) systems are ubiquitous in signal and image processing,
where they form the core of many algorithms. This library provides a syntax for
describing LSI systems and tools to analyze their behavior.

# Syntax

This library uses observable sharing to provide a convenient syntax for
describing LSI systems. For example, here is how a one-dimensional IIR filter:

```haskell
{-# LANGUAGE DataKinds #-}

import LSI

-- Simple one-dimensional IIR filter:
-- y(t) = x(t) + 0.5 x(t-1) + 0.3 x(t-2) - y(t-1) + 0.123 y(t-2)

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
```
