-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module LSI.Grid where

import Data.Array.Repa as R
import Data.Vector.Unboxed.Base


frequencyGrid1D :: forall f. (Unbox f, RealFloat f) => Int -> Array D DIM1 f
frequencyGrid1D n | n <= 1 = error "n should be greater >= 2"
frequencyGrid1D n = fromFunction (Z :. (2*n)) $
  let m = 2*pi :: f
      offset = m / 2 :: f
      step = m / (fromInteger . fromIntegral $ 2*n) :: f in
  \(R.Z R.:. x) ->
    ((fromInteger $ fromIntegral x)*step - offset)

frequencyGrid2D :: forall f. (Unbox f, RealFloat f) => Int -> Array D DIM2 (f, f)
frequencyGrid2D n | n <= 1 = error "n should be greater >= 2"
frequencyGrid2D n = fromFunction (Z :. (2*n) :. (2*n)) $
  let m = 2*pi :: f
      offset = m / 2 :: f
      step = m / (fromInteger . fromIntegral $ 2*n) :: f in
  \(R.Z R.:. x R.:. y) ->
    ( (fromInteger $ fromIntegral x)*step - offset
    , (fromInteger $ fromIntegral y)*step - offset
    )

frequencyGrid3D :: forall f. (Unbox f, RealFloat f) => Int -> Array D DIM3 (f, f, f)
frequencyGrid3D n | n <= 1 = error "n should be greater >= 2"
frequencyGrid3D n = fromFunction (Z :. (2*n) :. (2*n) :. (2*n)) $
  let m = 2*pi :: f
      offset = m / 2 :: f
      step = m / (fromInteger . fromIntegral $ 2*n) :: f in
  \(R.Z R.:. x R.:. y R.:. z) ->
    ( (fromInteger $ fromIntegral x)*step - offset
    , (fromInteger $ fromIntegral y)*step - offset
    , (fromInteger $ fromIntegral z)*step - offset
    )
