{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSI.Graph.Fgl where

import Data.Maybe (catMaybes, fromJust)
import Data.GraphViz
import Data.Reify.Graph (Graph(..))
import Data.List
import Data.Vector hiding ((++), map, concat)
import qualified Data.Vector as V
import GHC.TypeLits
import Linear.V (V(..))
import qualified Data.Graph.Inductive.Graph as FG
import qualified Data.Map as M
import qualified Data.Set as S

import qualified LSI.System as Sys

data Node c i = Input i | Sum | Mul c
  deriving (Eq, Show)

instance (Show c, Show i) => Labellable (Node c i) where
  toLabelValue v = case v of
    Input i -> toLabelValue (show i)
    Mul c -> toLabelValue ("*" ++ show c)
    Sum -> toLabelValue "+"

data DepVector = DepVector { unDep :: Vector Int}

instance Labellable DepVector where
  toLabelValue d = toLabelValue $
    concat $ [ "("
             , concat $ intersperse "," $
               map show $ V.toList $ unDep d
             , ")"
             ]

toFglGraph :: forall d i c gr.
  (KnownNat d, Show i, Show c, FG.Graph gr) => Sys.SystemGraph d i c -> gr (Node c i) DepVector
toFglGraph (Graph nodes root) = FG.mkGraph vs es
  where vs = catMaybes $ map addLabel nodes
        es = snd $ buildEdges root S.empty
        nodeMap = M.fromList nodes
        addLabel (i, x) = case x of
          Sys.Input l -> Just $ (i, Input l) -- (i, "Input: " ++ show l)
          Sys.Add _ _ -> Just $ (i, Sum) -- (i, "+")
          Sys.Mul c _ -> Just $ (i, Mul c) -- (i , "x "++ show c)
          Sys.Ref _ _ -> Nothing -- References do not appear as nodes

        -- Todo: using a state monad would be cleaner.
        buildEdges :: Int -> S.Set Int -> (S.Set Int, [FG.LEdge DepVector])
        buildEdges i visited | i `S.member` visited = (visited, [])
        buildEdges i visited =
          let visited' = S.insert i visited in
            case (fromJust $ M.lookup i nodeMap) of
              Sys.Input i -> (visited', [])
              Sys.Add a b -> let (v1, x) = deref a
                                 (v2, y) = deref b
                                 (visited3, es1) = buildEdges x visited'
                                 (visited4, es2) = buildEdges y visited3 in
                               (visited4,
                                (i, x, toDep v1):(i, y, toDep v2):
                                (es1 ++ es2)
                               )

              Sys.Mul _ a -> let (v, x) = deref a
                                 (visit, es) = buildEdges x visited' in
                               (visit, (i, x, toDep v):es)
              Sys.Ref a _ -> error "Invariant violation."

        deref = deref' S.empty 0
        toDep :: V d Int -> DepVector
        toDep = DepVector . toVector
        deref' visited acc i | i `S.member` visited = error "Circular reference."
        deref' visited acc i = case (fromJust $ M.lookup i nodeMap) of
          Sys.Ref j vec -> deref' (S.insert i visited) (acc+vec) j
          _ -> (acc, i)
