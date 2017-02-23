{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
                  )
where

import Control.Applicative (pure, (<$>), (<*>))
import Data.Reify
import GHC.TypeLits

import System.IO.Unsafe (unsafePerformIO)

type family Vec (d :: Nat) :: *

type instance Vec 1 = Int
type instance Vec 2 = (Int, Int)
type instance Vec 3 = (Int, Int, Int)

data System' (d::Nat) i c a =
  Input i       |
  Add a a       |
  Mul c a       |
  Ref a (Vec d)

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
(@:) a v = System $ Ref a v

input = System . Input

infixl 6 +:
infix  7 *:
infix  8 @:

type SystemGraph d i c = Graph (System' d i c)

toGraph :: System d i c -> SystemGraph d i c
toGraph  = unsafePerformIO . reifyGraph
