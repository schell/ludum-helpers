{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module FRP where

import Yarn
import Types
import UserInput
import Linear
import Gelatin (InputEvent(..),InputEnv(..),Key(..))
import Control.Monad.Reader
import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Set hiding (map)
import Data.Traversable

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
addVelocity :: Position -> Velocity -> Position
addVelocity p v = p ^+^ v
--------------------------------------------------------------------------------
-- Player character control signals
--------------------------------------------------------------------------------

keyboardVelocity :: Yarn (Reader InputEnv) a Velocity
keyboardVelocity = proc _ -> do
    uc <- keyboardControl -< ()
    let toV Left'  = V2 (-1) 0
        toV Right' = V2 1    0
        toV Up     = V2 0    (-1)
        toV Down   = V2 0    1
        toV None   = zero
    returnA -< (5 *^) $ sum $ map toV $ userDirections uc

keyboardControl :: Yarn (Reader InputEnv) a UserControl
keyboardControl = UserControl <$> keyboardDirections

keyboardDirections :: Yarn (Reader InputEnv) a [Direction]
keyboardDirections = catMaybes <$> sequenceA kvs
    where kvs = [ pure (Key'W, Up)     ~> whenKeyed
                , pure (Key'A, Left')  ~> whenKeyed
                , pure (Key'S, Down)   ~> whenKeyed
                , pure (Key'D, Right') ~> whenKeyed
                ]

whenKeyed :: Yarn (Reader InputEnv) (Key, a) (Maybe a)
whenKeyed = proc (k,a) -> do
    down <- keyIsDown -< k
    returnA -< if down then Just a else Nothing
--------------------------------------------------------------------------------
-- General input signals
--------------------------------------------------------------------------------
keyIsDown :: Yarn (Reader InputEnv) Key Bool
keyIsDown = valYarnM $ \k -> member k . ienvKeysDown <$> ask

getKey :: [InputEvent] -> Maybe InputEvent
getKey [] = Nothing
getKey (k@(KeyEvent _ _ _ _):_) = Just k
getKey (_:ks) = getKey ks
