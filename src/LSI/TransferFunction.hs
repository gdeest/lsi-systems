{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- # LANGUAGE ExplicitForAll #
module LSI.TransferFunction where

import LSI.RationalFunction (RationalFunction)
import LSI.System

import Data.Reify (Graph(..))

import GHC.TypeLits

import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified LSI.RationalFunction as RF

-- | Computes a map associating each node in the graph to its transfer function to the output.
transferFunctions :: (KnownNat d, Ord i, Num c, Show c)
                  => SystemGraph d i c -> IM.IntMap (RationalFunction d c)

transferFunctions (Graph nodes root) = computeTFs S.empty root
  where computeTFs baseNodes nodeId = IM.union (IM.fromList [(nodeId, RF.one)]) $
            case (nodeMap IM.! nodeId) of
              Input _   -> IM.empty
              Add n1 n2 -> let (tfs1, tfs2) = (getTFs n1, getTFs n2) in
                             solveForSelf $ IM.unionWith RF.Add tfs1 tfs2

              Mul c n   -> IM.map (RF.Mul (RF.constant c)) (getTFs n)
              Ref n d   -> IM.map (RF.Mul $ RF.Monomial 1 d) (getTFs n)

          where getTFs nodeId' = if nodeId' `S.member` baseNodes'
                                 then IM.fromList [(nodeId', RF.one)]
                                 else computeTFs baseNodes' nodeId'
                baseNodes' = S.insert nodeId baseNodes

                -- Given a map of transfer functions to a node remove the current node
                -- from the map by dividing every other transfer function by (1-TF).
                solveForSelf tfMap = case IM.lookup nodeId tfMap of
                  Just tf -> let denom = RF.Add RF.one (RF.Mul (RF.constant (-1)) tf) in
                               IM.map (flip RF.Div denom) (IM.delete nodeId tfMap)
                  Nothing -> tfMap

        nodeMap  = IM.fromList nodes
