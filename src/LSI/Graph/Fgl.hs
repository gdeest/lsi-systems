{-# LANGUAGE DataKinds #-}
module LSI.Graph.Fgl where

import Data.Maybe (catMaybes, fromJust)
import Data.Reify.Graph (Graph(..))
import GHC.TypeLits
import qualified Data.Graph.Inductive.Graph as FG
import qualified Data.Map as M
import qualified Data.Set as S

import LSI.System

toFglGraph :: (KnownNat d, Show i, Show c, FG.Graph gr) => SystemGraph d i c -> gr String String
toFglGraph (Graph nodes root) = FG.mkGraph vs es
  where vs = catMaybes $ map addLabel nodes
        es = buildEdges root S.empty
        nodeMap = M.fromList nodes
        addLabel (i, x) = case x of
          Input l -> Just $ (i, "Input: " ++ show l)
          Add _ _ -> Just $ (i, "+")
          Mul c _ -> Just $ (i , "x "++ show c)
          Ref _ _ -> Nothing -- References do not appear as nodes

        buildEdges :: Int -> S.Set Int -> [FG.LEdge String]
        buildEdges i visited | i `S.member` visited = []
        buildEdges i visited =
          let visited' = S.insert i visited in
            case (fromJust $ M.lookup i nodeMap) of
              Input i -> []
              Add a b -> let (v1, x) = deref a
                             (v2, y) = deref b in
                           (i, x, show v1):(i, y, show v2):
                           (concat [ buildEdges x visited'
                                   , buildEdges y visited' ])

              Mul _ a -> let (v, x) = deref a in
                           (i, x, show v):(buildEdges x visited')
              Ref a _ -> error "Invariant violation."

        deref = deref' S.empty 0
        deref' visited acc i | i `S.member` visited = error "Circular reference."
        deref' visited acc i = case (fromJust $ M.lookup i nodeMap) of
          Ref j vec -> deref' (S.insert i visited) (acc+vec) j
          _ -> (acc, i)
