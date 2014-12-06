{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Types where

import Yarn
import Linear
import Control.Lens
import Data.Typeable
import Control.Eff.State.Strict
import qualified Data.IntMap as IM


newtype ID = ID { unID :: Int } deriving (Show, Read, Eq, Ord, Typeable, Enum, Num)
newtype Player = Player ID deriving (Typeable)
type Component a = IM.IntMap a
type Entity a = State (Component a)
--type CanRead a r = Member (Reader a) r
--type CanModify a r = Member (State a) r
type VaryingComponent m a = Component (Varying m a)
type Varying m a = Yarn m () a

type Color = V4 Float
type Width = Float
type Height = Float
type HalfWidth = Float
type HalfHeight = Float
type Rotation = Float
type SeparatingAxis = V2 Float
type Line = (V2 Float, V2 Float)
type Position = V2 Float
type PositionOffset = V2 Float
type Velocity = V2 Float
newtype Name = Name String deriving (Show, Typeable)

data AABB = AABB { aabbPosition :: Position
                 , aabbHalfWidth :: HalfWidth
                 , aabbHalfHeight :: HalfHeight
                 } deriving (Show, Eq, Ord, Typeable)
makeLensesFor [("aabbPosition", "aabbPositionLens")
              ,("aabbHalfWidth", "aabbHalfWidthLens")
              ,("aabbHalfHeight", "aabbHalfHeightLens")
              ] ''AABB

data Mass = Kilos Float
          | Immobile
          deriving (Typeable, Eq, Ord, Show)

massOf :: Mass -> Float
massOf (Kilos k) = k
massOf _ = 0

data PhysicalBody = PhysicalBody { pbAABB :: AABB
                                 , pbMass :: Mass
                                 } deriving (Typeable, Eq, Ord, Show)
makeLensesFor [("pbAABB", "pbAABBLens")
              ,("pbMass", "pbMassLens")
              ] ''PhysicalBody


isMobile :: PhysicalBody -> Bool
isMobile (PhysicalBody _ Immobile) = False
isMobile _ = True

data Quadtree a = Quadtree { qtBounds :: AABB
                           , qtItems  :: [(AABB, a)]
                           , qtQuads  :: Maybe ( Quadtree a
                                               , Quadtree a
                                               , Quadtree a
                                               , Quadtree a
                                               )
                           } deriving (Show, Eq, Ord, Typeable)

data TimeDelta = Float
data Direction = Up
               | Down
               | Left'
               | Right'
               | None
               deriving (Eq, Ord, Show, Typeable)
