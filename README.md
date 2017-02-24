# Introduction

Linear, shift-invariant (LSI) systems are ubiquitous in signal and image processing,
where they form the core of many algorithms. This library provides a syntax for
describing LSI systems and tools to analyze their behavior.

# Syntax

This library uses observable sharing to provide a convenient syntax for
describing LSI systems. For example, here is how a one-dimensional IIR filter can be expressed:

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

# Transformation to Explicit Graph

The IIR filter above can be transformed to an explicit (non-recursive) graph representation using the `toGraph` function. Each node in the output is represented as a unique integer:

```haskell
> g = toGraph iir
> g
let [(1,Add 2 12),(12,Mul 0.123 13),(13,Ref 1 (V {toVector = [-2]})),(2,Add 3 10),(10,Mul (-1.0) 11),(11,Ref 1 (V {toVector = [-1]})),(3,Add 4 8),(8,Mul 0.3 9),(9,Ref 5 (V {toVector = [-2]})),(4,Add 5 6),(6,Mul 0.5 7),(7,Ref 5 (V {toVector = [-1]})),(5,Input "x")] in 1
```

# Transfer Function Computation

We can compute transfer functions of multi-dimensional systems using the
`transferFunctions` function from `LSI.TransferFunction`. The result is returned
as a map from each node to the output of the system, which is quite handy if you
want to analyze the impact of roundoff noise on the output. Transfer functions
are expressed as (unsimplified) rational functions of multivariate Laurent
polynomials.

For example, noting that the input node is given the label 5 in the above
section, we can get its transfer function to the output:

```haskell
> import qualifie Data.IntMap as IM
> (transferFunctions g) IM.! 5
Div (Add (Add (Monomial 1.0 (V {toVector = [0]})) (Mul (Monomial 0.5 (V {toVector = [0]})) (Mul (Monomial 1.0 (V {toVector = [-1]})) (Monomial 1.0 (V {toVector = [0]}))))) (Mul (Monomial 0.3 (V {toVector = [0]})) (Mul (Monomial 1.0 (V {toVector = [-2]})) (Monomial 1.0 (V {toVector = [0]}))))) (Add (Monomial 1.0 (V {toVector = [0]})) (Mul (Monomial (-1.0) (V {toVector = [0]})) (Add (Mul (Monomial (-1.0) (V {toVector = [0]})) (Mul (Monomial 1.0 (V {toVector = [-1]})) (Monomial 1.0 (V {toVector = [0]})))) (Mul (Monomial 0.123 (V {toVector = [0]})) (Mul (Monomial 1.0 (V {toVector = [-2]})) (Monomial 1.0 (V {toVector = [0]})))))))
```
