{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
module LSI.TransferFunction where

import LSI.RationalFunction (RationalFunction)
import LSI.System

import Data.Reify
import Data.Function.Memoize
import GHC.TypeLits

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified LSI.RationalFunction as RF

transferFunctions :: forall (d::Nat) c i. Ord c => SystemGraph d c i -> IM.IntMap (RationalFunction d c)
transferFunctions (Graph nodes root) = transferFunctions' S.empty root
  where node_map = IM.fromList nodes
        inputMap = foldr addInput M.empty nodes
          where addInput (uid, n) m = case n of
                  Input name -> M.insert name uid m
                  _          -> m

        -- memoTF = memoize2 transferFunctions'

        transferFunctions' :: S.Set Unique -> Unique -> IM.IntMap (RationalFunction c d)
        transferFunctions' baseNodes nodeId = case (node_map IM.! nodeId) of
          -- Base case: the node is an input ; the transfer function is the identity.
          Input name -> IM.fromList [(inputMap M.! name, RF.One)]
          Add x y -> undefined
          Mul k x -> undefined
          Ref x d -> undefined

          -- Non input node: the transfer functions of each summand are computed
          -- recursively in terms of input and base nodes, the current node
          -- being added to the list of base nodes.
          --
          -- The transfer functions are then grouped by node and the resulting
          -- transfer function is computed by dividing each other transfer
          -- function by (1-TF), where TF is the transfer function of the
          -- recursive part on the RHS.
          -- Node xs    -> solveForSelf tfMap
          --   where
          --     -- We add the current node to the list of base nodes.
          --     baseNodes' = S.insert nodeId baseNodes

          --     -- Memoized version of transferFunctions' curried with the new set of base nodes.
          --     memo       = memoize $ transferFunctions' baseNodes'

          --     -- Get rid of self reference on the RHS.
          --     solveForSelf m = case IM.lookup nodeId m of
          --       Just tf -> let denom = Sum unitRat (Mul (constantRat (-1)) tf) in
          --                    IM.map (flip Div denom) (IM.delete nodeId m)
          --       Nothing -> m

          --     -- Map of transfer functions in terms of input and base nodes.
          --     tfMap :: IM.IntMap RationalFunction
          --     tfMap = sumTFs $ map toTFMap xs

          --     -- Merge all transfer functions by summing those corresopnding to
          --     -- the same variable.
          --     sumTFs :: [IM.IntMap RationalFunction] -> IM.IntMap RationalFunction
          --     sumTFs = foldr addMap IM.empty

          --     addMap :: IM.IntMap RationalFunction -> IM.IntMap RationalFunction -> IM.IntMap RationalFunction
          --     addMap = IM.foldrWithKey addRat

          --     addRat :: Unique -> RationalFunction -> IM.IntMap RationalFunction -> IM.IntMap RationalFunction
          --     addRat  = IM.insertWith Sum

          --     -- Convert a (coeff, delay, nodeid) tuple to the corresponding set
          --     -- of transfer functions wrt. to the current base nodes.
          --     toTFMap :: (Float, Delay, Unique) -> IM.IntMap RationalFunction
          --     toTFMap (k, (d1,d2), nid) = IM.map (\rf ->  Mul (Monomial k d1 d2) rf) $
          --       if nid `S.member` baseNodes' then IM.fromList [(nid, unitRat)]
          --       else traceShow "Call memo" $ memo nid

