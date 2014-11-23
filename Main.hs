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
import Rendering
import FRP
import Yarn hiding (forever)
import Entity
import Data.Time.Clock
import Data.Typeable
import Data.Monoid
import Data.Traversable
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
              , Member (Entity Name) r
              , Member (Entity (Varying (R.Reader InputEnv) Position)) r
              , Member (Entity (Varying (R.Reader InputEnv) Velocity)) r
              )
           => Eff r ()
initialize = do
    [reaper, pinky] <- replicateM 2 fresh
    let x :: Yarn (R.Reader InputEnv) () Float
        x = easeOutExpo 0 300 2 `andThen` easeOutExpo 300 0 2 `andThen` x
        p = Position <$> (V2 <$> x <*> x)
    -- looks like a reaper moving south
    reaper `addProperty` (Reaper South)
    -- position changes over time according to some signal
    reaper `addProperty` (p :: Varying (R.Reader InputEnv) Position)
    -- is green witha  transparent background
    reaper `addProperty` Colors green transparent

    pinky `addProperty` (Reaper East)
    pinky `addProperty` (Position $ V2 100 10)
    pinky `addProperty` playerKeyVelocities
    pinky `addProperty` Colors pink transparent
    pinky `addProperty` Name (concat [row1, "\n", row2, "\n", row3])
        where row1 = [' ' .. '@']
              row2 = ['A' .. '`']
              row3 = ['a' .. '~']

play :: ( Member (Fresh ID) r
        , Member (Entity Displayable) r
        , Member (Entity Colors) r
        , Member (Entity Position) r
        , Member (Entity Name) r
        , Member (Entity (Varying (R.Reader InputEnv) Position)) r
        , Member (Entity (Varying (R.Reader InputEnv) Velocity)) r
        , Member (Reader WindowRef) r
        , Member (Reader Renderer) r
        , Member (State UTCTime) r
        , Member (State InputEnv) r
        , SetMember Lift (Lift IO) r
        )
       => Eff r ()
play = do
    -- Tick time.
    t' <- lift $ getCurrentTime
    t  <- get
    put t'
    let dt = realToFrac $ diffUTCTime t' t :: Double

    -- Get the user events and fold them into our InputEnv.
    loadNewEvents

    --- Progress varying position
    (input :: InputEnv) <- get
    (pvars :: Component (Varying (R.Reader InputEnv) Position)) <- get

    let pread y = R.runReader (stepYarn y dt ()) input
        pouts   = pread <$> pvars
        pvals   = outVal  <$> pouts
        pvars'  = outYarn <$> pouts

    ---- Update varying positions
    put pvars'
    -- Update static positions
    modify $ IM.union pvals

    -- Progress varying velocity
    pos <- get
    (vvars :: Component (Varying (R.Reader InputEnv) Velocity)) <- get

    let vread y = R.runReader (stepYarn y dt ()) input
        vouts   = vread <$> vvars
        vvals   = outVal  <$> vouts
        vvars'  = outYarn <$> vouts
        pos'    = IM.intersectionWith incrementPosition vvals pos

    lift $ print pos'
    ---- Update varying positions
    put vvars'

    ---- Update static positions
    modify $ IM.union pos'

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
            $ evalState (mempty :: Component Colors)
            $ evalState (mempty :: Component Name)
            $ evalState (mempty :: Component (Varying (R.Reader InputEnv) Velocity))
            $ evalState (mempty :: Component (Varying (R.Reader InputEnv) Position))
            $ evalState emptyInputEnv
            $ evalState t
            $ flip runReader wref
            $ flip runReader renderer
            $ flip runFresh (ID 0) $ do
                initialize
                forever play
