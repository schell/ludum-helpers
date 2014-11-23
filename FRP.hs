{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module FRP where

import Entity
import Yarn
import Gelatin hiding (getKey, get)
import Control.Monad.Reader
import Control.Applicative
import Control.Arrow
import Data.Set hiding (map)
import Data.Traversable

--------------------------------------------------------------------------------
-- Player character control signals
--------------------------------------------------------------------------------
playerKeyVelocities :: Yarn (Reader InputEnv) () Velocity
playerKeyVelocities = Velocity . sum . map unvelocity <$> sequenceA kvs
    where kvs = [ pure (Key'K, Velocity $ V2 0 (-5)) ~> keyedVelocity
                , pure (Key'J, Velocity $ V2 0 5) ~> keyedVelocity
                , pure (Key'L, Velocity $ V2 5 0) ~> keyedVelocity
                , pure (Key'H, Velocity $ V2 (-5) 0) ~> keyedVelocity
                ]

keyedVelocity :: Yarn (Reader InputEnv) (Key, Velocity) Velocity
keyedVelocity = proc (k,v) -> do
    down <- keyIsDown -< k
    returnA -< if down then v else Velocity zero
--------------------------------------------------------------------------------
-- General input signals
--------------------------------------------------------------------------------
keyIsDown :: Yarn (Reader InputEnv) Key Bool
keyIsDown = valYarnM $ \k -> member k . ienvKeysDown <$> ask

getKey :: [InputEvent] -> Maybe InputEvent
getKey [] = Nothing
getKey (k@(KeyEvent _ _ _ _):_) = Just k
getKey (_:ks) = getKey ks
