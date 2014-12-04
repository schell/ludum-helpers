{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Gelatin hiding (drawArrays, get, Position, renderer, Name)
import Types
import Toons
import UserInput
import Rendering
import Yarn hiding (forever)
import Entity
import Data.Time.Clock
import Data.Typeable
import Data.Monoid
import Data.Traversable
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Control.Monad (forever, replicateM)
import qualified Control.Monad.Reader as R
import Control.Applicative
import Control.Concurrent
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict

deriving instance Typeable (R.ReaderT)

initialize :: ( Member (Fresh ID) r
              , Member (Entity Displayable) r
              , Member (Entity Colors) r
              , Member (Entity Position) r
              , Member (Entity (PlayerControl Key)) r
              , Member (Entity (PlayerControl Int)) r
              )
           => Eff r ()
initialize = do
    [reaper, pinky] <- replicateM 2 fresh

    reaper `addProperty` (Reaper South)
    reaper `addProperty` (V2 0 0 :: Position)
    reaper `addProperty` Colors green transparent
    reaper `addProperty` (playerControl wasdKeyMap)

    pinky `addProperty` (Reaper East)
    pinky `addProperty` (V2 0 0 :: Position)
    pinky `addProperty` Colors pink transparent
    pinky `addProperty` (playerControl dpadKeyMap)

-- Find entities that have keyboard control and progress their positions.
stepKeyboardControlledPositions dt = do
    (input :: InputEnv) <- get
    (positions :: Component Position) <- get
    (controls :: Component (PlayerControl Key)) <- get

    let positions' = IM.intersectionWith (\ctrl pos -> ctrl (ienvKeysDown input) dt + pos) controls positions
    modify $ IM.union positions'

-- Find entities that have joystick control and progress their positions.
stepJoystickControlledPositions dt = do
    mJInput <- lift $ getJoystickInput Joystick'1
    let f s (b, i) = if b == JoystickButtonState'Pressed then S.insert i s else s
        buttonSet = case mJInput of
                        Nothing -> S.empty
                        Just ji -> foldl f S.empty $ zip (jiButtons ji) [0..]
    lift $ print buttonSet
    (positions :: Component Position) <- get
    (controls :: Component (PlayerControl Int)) <- get

    let positions' = IM.intersectionWith (\ctrl pos -> ctrl buttonSet dt + pos) controls positions
    modify $ IM.union positions'


play = do
    -- Tick time.
    t' <- lift $ getCurrentTime
    t  <- get
    put t'
    let dt = realToFrac $ diffUTCTime t' t :: Float

    -- Get the user events and fold them into our InputEnv.
    loadNewEvents

    stepKeyboardControlledPositions dt
    stepJoystickControlledPositions dt

                                     -- Progress the signals
    --let stepVaryingComponent mf = do vars <- get
    --                                 let runM y = mf $ stepYarn y dt ()
    --                                     outs   = runM <$> vars
    --                                     vals   = outVal  <$> outs
    --                                     vars'  = outYarn <$> outs
    --                                 -- Update the signals
    --                                 put vars'
    --                                 -- Return the static values
    --                                 return vals

    -- Progress varying positions
    --(pvals :: Component Position) <- stepVaryingComponent (flip R.runReader input)
    -- Update static positions
    --modify $ IM.union pvals

    ---- Update static positions
    --modify $ IM.union pos'
    vvals <- get

    -- Use our velocities to set some displayables
    displayables <- get
    let displayables' = IM.intersectionWith displayPlusVelocity displayables vvals
    modify $ IM.union displayables'

    -- Display our game
    displayAll
    -- Clear out the list of events that happened this frame.
    clearLastEvents
    -- Handle the possibility of quitting.
    handleQuit
    -- Pass some time so we don't hog all the CPU cycles.
    lift $ threadDelay 100

main :: IO ()
main = do
    wref     <- initWindow (V2 300 600) (V2 600 600) "ludum-helpers"
    renderer <- newRenderer =<< getWindow wref
    t        <- getCurrentTime

    runLift $ evalState (mempty :: Component Displayable)
            $ evalState (mempty :: Component Position)
            $ evalState (mempty :: Component (Varying (R.Reader InputEnv) Position))
            $ evalState (mempty :: Component Velocity)
            $ evalState (mempty :: Component Colors)
            $ evalState (mempty :: Component Name)
            $ evalState (mempty :: Component (PlayerControl Key))
            $ evalState (mempty :: Component (PlayerControl Int))
            $ evalState emptyInputEnv
            $ evalState t
            $ flip runReader wref
            $ flip runReader renderer
            $ flip runFresh (ID 0) $ do
                initialize
                forever play
