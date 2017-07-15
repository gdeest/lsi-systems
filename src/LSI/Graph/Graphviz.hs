{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LSI.Graph.Graphviz where

import Data.GraphViz
import GHC.TypeLits
import qualified Data.Graph.Inductive.PatriciaTree as PT
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as T

import LSI.Graph.Fgl
import LSI.System

toDotSource :: forall d i c. (KnownNat d, Show i, Show c) => SystemGraph d i c -> T.Text
toDotSource g =
  printDotGraph dotg
  where g' = toFglGraph g :: PT.Gr (Node c i) DepVector
        dotg = graphToDot quickParams g'

toDotFile :: (KnownNat d, Show i, Show c) => String -> SystemGraph d i c -> IO ()
toDotFile fname = (TIO.writeFile fname) . T.toStrict . toDotSource
