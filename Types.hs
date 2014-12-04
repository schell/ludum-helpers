{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Linear
import Data.Typeable

data TimeDelta = Float
data Direction = Up
               | Down
               | Left'
               | Right'
               | None
               deriving (Eq, Ord, Show, Typeable)

--------------------------------------------------------------------------------
-- Components (not a complete list)
--------------------------------------------------------------------------------
type Position = V2 Float
type Velocity = V2 Float
newtype Name = Name String deriving (Show, Typeable)

--unvelocity :: Velocity -> V2 Float
--unvelocity (Velocity v) = v
--
--unposition :: Position -> V2 Float
--unposition (Position p) = p
