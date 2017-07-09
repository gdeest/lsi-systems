{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LSI.System ( Vec
                  , System'(..)
                  , System(..)
                  , (+:)
                  , (*:)
                  , (@:)
                  , input
                  , SystemGraph
                  , toGraph
                  , off
                  , dot
                  , sum'
                  )
where

import Control.Applicative (pure, (<$>), (<*>))
import Data.Reify
import GHC.TypeLits
import Linear.V

import qualified Data.Vector as V

import System.IO.Unsafe (unsafePerformIO)

class ToV v (n::Nat) | v -> n, n -> v where
  toV :: v -> V n Int

instance ToV Int 1 where
  toV i = V (V.fromList [i])

instance ToV (Int,Int) 2 where
  toV (i,j) = V (V.fromList [i,j])

instance ToV (Int,Int,Int) 3 where
  toV (i,j,k) = V (V.fromList [i,j,k])

type family Vec (d :: Nat) :: *

type instance Vec 1 = Int
type instance Vec 2 = (Int, Int)
type instance Vec 3 = (Int, Int, Int)
type instance Vec 4 = (Int, Int, Int, Int)
type instance Vec 5 = (Int, Int, Int, Int, Int)
type instance Vec 6 = (Int, Int, Int, Int, Int, Int)

data System' (d::Nat) i c a =
  Input i       |
  Add a a       |
  Mul c a       |
  Ref a (V d Int)

deriving instance (Show (Vec d), Show i, Show c, Show a) => Show (System' d i c a)

newtype System d i c = System (System' d i c (System d i c))

instance MuRef (System d i c) where
  type DeRef (System d i c) = System' d i c

  mapDeRef f (System body) = case body of
    Input i -> pure (Input i)
    Add a b -> Add <$> f a <*> f b
    Mul c a -> Mul <$> pure c <*> f a
    Ref a v -> Ref <$> f a <*> pure v

(+:) a b = System $ Add a b
(*:) c a = System $ Mul c a
(@:) a v = System $ Ref a (toV v)

input = System . Input

infixl 6 +:
infix  7 *:
infix  8 @:

off xs ds = map (\(x,d) -> x @: d) $ zip xs ds
dot ks xs = sum' $ map(\(k,x) -> k *: x) $ zip ks xs

sum' (x:xs) = foldr (+:) x xs

type SystemGraph d i c = Graph (System' d i c)

toGraph :: System d i c -> SystemGraph d i c
toGraph  = unsafePerformIO . reifyGraph
