{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- import Data.Array.Repa (Array, U, DIM2)
import qualified Data.Array.Repa as R
import qualified Data.IntMap as IM
import Control.Monad
import Data.Complex
import qualified Data.Vector as V
import qualified Linear.V as V
import LSI
import LSI.Examples
import LSI.FrequencyResponse
import LSI.Grid
import LSI.RationalFunction
import LSI.TransferFunction
import Unsafe.Coerce (unsafeCoerce)

graph :: SystemGraph 2 String Double
graph = toGraph $ deriche_ydiff 1.0

tfs :: IM.IntMap (RationalFunction 2 Double)
tfs = IM.map simplify $ transferFunctions graph

frequencyResponses :: IM.IntMap (V.V 2 Double -> Complex Double)
frequencyResponses = IM.map computeFR tfs

n :: Num a => a
n = 50

grid :: R.Array R.D R.DIM2 (Double, Double)
grid = frequencyGrid2D n

nPoints :: Num a => Double
nPoints = (2*n)^2

plotFR :: (V.V 2 Double -> Complex Double) -> R.Array R.D R.DIM2 (Complex Double)
plotFR fr = R.map (fr . V.V . V.fromList . \(w1,w2) -> [w1,w2]) grid

l2Norm :: R.Array R.D R.DIM2 (Complex Double) -> IO Double
l2Norm plot = fmap (/ nPoints) $ R.foldAllP (+) 0 squares
  where squares = R.map (\x -> realPart $ (abs x)^2) plot

plots :: IM.IntMap (R.Array R.D R.DIM2 (Complex Double))
plots = IM.map plotFR $ unsafeCoerce frequencyResponses -- TODO: Kind mismatch.

main :: IO ()
main = do
  -- print grid
  -- let plot = plots IM.! 13
  -- forM_ [0..2*n+1] $ \(i::Int) ->
  --   forM_ [0..2*n+1] $ \(j::Int) -> do
  --     let (x,y,z) = (i,j, (^ 2) . realPart . abs $ plot R.! (R.Z R.:. i R.:. j))
  --     putStrLn $ show i ++ ", " ++ show j ++ ", " ++ show z
  -- print $ toMatlabFunction $ tfs IM.! 13
  -- norm <- l2Norm plot
  -- print norm

  let ps = IM.elems plots
  norms <- forM ps $ \p -> l2Norm p
  print norms
