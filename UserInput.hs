{-# LANGUAGE DeriveDataTypeable #-}
module UserInput where

import Linear
import Types
import Data.Typeable
import Data.Maybe
import Graphics.UI.GLFW (Key(..))
import qualified Data.Set as S

type Time = Float
type KeyMap a = [(a, Direction)]
type PlayerControl a = (S.Set a -> Time -> Velocity)
data AIControl = AIControl deriving (Show, Typeable)

wasdKeyMap :: KeyMap Key
wasdKeyMap = [ (Key'W, Up)
             , (Key'A, Left')
             , (Key'S, Down)
             , (Key'D, Right')
             ]

dpadKeyMap :: KeyMap Int
dpadKeyMap = [ (12, Up)
             , (15, Left')
             , (14, Down)
             , (13, Right')
             ]



playerControl :: Eq a => KeyMap a -> PlayerControl a
playerControl keyMap keys t = t *^ v
    where v = (100 *^) $ sum $ map toV directions
          directions = catMaybes $ S.toList $ S.map (`lookup` keyMap) keys
          toV Left'  = V2 (-1) 0
          toV Right' = V2 1    0
          toV Up     = V2 0    (-1)
          toV Down   = V2 0    1
          toV None   = zero
