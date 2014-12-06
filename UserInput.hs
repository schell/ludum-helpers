{-# LANGUAGE DeriveDataTypeable #-}
module UserInput where

import Linear
import Types
import Data.Typeable
import Data.Maybe
import Graphics.UI.GLFW (Key(..))
import qualified Data.Set as S

type Time = Float
type DirectionMap a = [(a, Direction)]
type RotationMap a = [(a, Rotation)]
type PlayerDirection a = (S.Set a -> Time -> Velocity)
type PlayerRotation a = (S.Set a -> Time -> Rotation)
data AIControl = AIControl deriving (Show, Typeable)

wasdDirectionMap :: DirectionMap Key
wasdDirectionMap =
    [ (Key'W, Up)
    , (Key'A, Left')
    , (Key'S, Down)
    , (Key'D, Right')
    ]

dpadDirectionMap :: DirectionMap Int
dpadDirectionMap =
    [ (12, Up)
    , (15, Left')
    , (14, Down)
    , (13, Right')
    ]

bumperRotationMap :: RotationMap Int
bumperRotationMap =
    [ (4, -1)
    , (5, 1)
    ]

playerDirection :: Eq a => Float -> DirectionMap a -> PlayerDirection a
playerDirection speed keyMap keys t = t *^ v
    where v = (speed *^) $ sum $ map toV directions
          directions = catMaybes $ S.toList $ S.map (`lookup` keyMap) keys
          toV Left'  = V2 (-1) 0
          toV Right' = V2 1    0
          toV Up     = V2 0    (-1)
          toV Down   = V2 0    1
          toV None   = zero

playerRotation :: Eq a => RotationMap a -> PlayerRotation a
playerRotation keyMap keys t = t * r
    where r = sum rotations
          rotations = catMaybes $ S.toList $ S.map (`lookup` keyMap) keys

