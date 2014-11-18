{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding (foldl, mapM_, sequence_)
import Gelatin hiding (drawArrays, get, Position, renderer, Name)
import Rendering
import Yarn hiding (forever)
import Entity
import Data.Time.Clock
import Data.Monoid
import Control.Monad hiding (mapM_, sequence_)
import Control.Applicative
import Control.Concurrent
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict

initialize :: ( Member (Fresh ID)                      r
              , Member (Entity Position)               r
              , Member (Entity (TimeVarying Position)) r
              , Member (Entity Name)                   r
              , Member (Entity Colors)                 r)
           => Eff r ()
initialize = do
    [reaper, pinky] <- replicateM 2 fresh
    let x = easeOutExpo 0 300 2 `andThen` easeOutExpo 300 0 2 `andThen` x
        v = V2 <$> x <*> x
    reaper `addProperty` (v :: TimeVarying Position)
    reaper `addProperty` Colors green transparent

    pinky `addProperty` (V2 100 10 :: Position)
    pinky `addProperty` Colors pink transparent
    pinky `addProperty` Name "00000011111222222"

play :: ( SetMember Lift (Lift IO)               r
        , Member (Fresh ID)                      r
        , Member (State UTCTime)                 r
        , Member (Entity (TimeVarying Position)) r
        , Member (Entity (TimeVarying Colors))   r
        , Member (Entity Position)               r
        , Member (Entity Colors)                 r
        , Member (Entity Name)                   r
        , Member (State InputEnv)                r
        , Member (Reader WindowRef)              r
        , Member (Reader Renderer)               r)
     => Eff r ()
play = do
    initialize
    forever $ do
        -- Get the user events and fold them into our InputEnv.
        loadNewEvents
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

    runLift $ evalState (mempty :: Component Position)
            $ evalState (mempty :: Component Colors)
            $ evalState (mempty :: Component Name)
            $ evalState (mempty :: Component (TimeVarying Position))
            $ evalState (mempty :: Component (TimeVarying Colors))
            $ evalState emptyInputEnv
            $ evalState t
            $ flip runReader wref
            $ flip runReader renderer
            $ runFresh play (ID 0)
