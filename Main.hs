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

import Prelude hiding (sequence_, minimum)
import Gelatin hiding (drawArrays, get, Position, renderer, Name)
import Yarn
import Types
import Toons
import UserInput
import Rendering
import Collision
import Data.Time.Clock
import Data.Typeable
import Data.Monoid
import Data.Foldable
import Data.List (sortBy)
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Control.Monad.Reader as R
import Control.Lens
import Control.Concurrent
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Control.Applicative
import System.Exit

deriving instance Typeable (R.ReaderT)
deriving instance Typeable Identity

initialize = do
    let blockPos = V2 150 150 :: Position
    block <- fresh
    block `addProperty` (Box 20 20)
    block `addProperty` (Colors transparent green)
    block `addProperty` (PhysicalBody (AABB blockPos 20 20) $ Kilos 50)
    block `addProperty` (0 :: Rotation)

    let playerPos = (V2 150 200)
        hovering  = V2 <$> 0 <*> (updown :: Yarn Identity () Float)
        updown    = linear 0 (-5) (1 :: Float) `andThen`
                      linear (-5) 0 (1 :: Float) `andThen` updown
    player <- fresh
    player `addProperty` (Reaper East)
    player `addProperty` (0 :: Rotation)
    player `addProperty` Colors pink transparent
    player `addProperty` (PhysicalBody (AABB playerPos 5 14) $ Kilos 10)
    player `addProperty` playerPos
    player `addProperty` (playerDirection 150 dpadDirectionMap)
    player `addProperty` (playerDirection 150 wasdDirectionMap)
    player `addProperty` hovering
    put $ Player player -- Setting our player as the player id

-- Progress the signals
stepVaryingComponent dt mf = do
    vars <- get
    let runM y = mf $ stepYarn y dt ()
        outs   = runM <$> vars
        vals   = outVal  <$> outs
        vars'  = outYarn <$> outs
    -- Update the signals
    put vars'
    -- Return the static values
    return vals

-- Find entities that have keyboard control and progress their positions.
stepKeyboardControlledThings dt = do
    (input :: InputEnv) <- get
    (positions :: Component Position) <- get
    (controls :: Component (PlayerDirection Key)) <- get

    let positions' = IM.intersectionWith (\ctrl pos -> ctrl (ienvKeysDown input) dt + pos) controls positions
    modify $ IM.union positions'

-- Find entities that have joystick control and progress their positions.
stepJoystickControlledThings dt = do
    mJInput <- lift $ getJoystickInput Joystick'1
    let f s (b, i) = if b == JoystickButtonState'Pressed then S.insert i s else s
        buttonSet = case mJInput of
                        Nothing -> S.empty
                        Just ji -> Prelude.foldl f S.empty $ zip (jiButtons ji) [0..]
    --lift $ print buttonSet
    (positions :: Component Position) <- get
    (poscontrols :: Component (PlayerDirection Int)) <- get

    (rotations :: Component Rotation) <- get
    (rotcontrols :: Component (PlayerRotation Int)) <- get

    let positions' = IM.intersectionWith (\ctrl pos -> ctrl buttonSet dt + pos)
                                         poscontrols
                                         positions
        rotations' = IM.intersectionWith (\ctrl rot -> ctrl buttonSet dt + rot)
                                         rotcontrols
                                         rotations
    modify $ IM.union positions'
    modify $ IM.union rotations'

-- Update physical bodies with positions.
updatePhysicalBodyPositions = do
    oldPositions <- get
    oldBodies <- get
    let newBodies = IM.intersectionWith (\p b -> b & pbAABBLens.aabbPositionLens .~ p)
                                        oldPositions
                                        oldBodies
    modify $ IM.union newBodies

-- Collide player with the environment.
collidePlayer = do
    (Player (ID player)) <- get
    (bodies :: Component PhysicalBody) <- get
    let mupdate  = do pbody <- IM.lookup player bodies
                      let bodies' = foldCollision (player, pbody) $
                                      IM.toList bodies

                      return $ put $ IM.fromList bodies'
    case mupdate of
        Nothing -> return ()
        Just f  -> f

play = do
    -- Tick time.
    t' <- lift $ getCurrentTime
    t  <- get
    put t'
    let dt = realToFrac $ diffUTCTime t' t :: Float

    -- Get the user events and fold them into our InputEnv.
    loadNewEvents

    -- Get the player position from last frame.
    Player (ID player) <- get
    mlastPlayerPos <- IM.lookup player <$> get

    stepKeyboardControlledThings dt
    stepJoystickControlledThings dt
    -- Get the player position after user input.

    -- Update the player's toon based on velocity
    mnewPlayerPos <- IM.lookup player <$> get
    mPlayerToon   <- IM.lookup player <$> get
    let mv = do lastPlayerPos <- mlastPlayerPos
                newPlayerPos  <- mnewPlayerPos
                playerToon    <- mPlayerToon
                let v = newPlayerPos - lastPlayerPos
                    toon = displayPlusVelocity playerToon v
                if v == zero then Nothing
                  else return $ do (ds :: Component Displayable) <- get
                                   put $ IM.insert player toon ds
                                   lift $ print (playerToon, toon)
    maybe (return ()) id mv

    updatePhysicalBodyPositions
    collidePlayer

    -- Update positions with new physical bodies.
    (positions :: Component Position) <- get
    bodies <- get
    let newPositions = IM.intersectionWith (const (^. pbAABBLens.aabbPositionLens))
                                           positions
                                           bodies
    -- Update old positions with new physical bodies
    modify $ IM.union newPositions

    -- Display our game
    displayAll dt

    -- Clear out the list of events that happened this frame.
    clearLastEvents
    -- Handle the possibility of quitting.
    handleQuit
    -- Pass some time so we don't hog all the CPU cycles.
    lift $ threadDelay 100

--------------------------------------------------------------------------------
-- Entities
--------------------------------------------------------------------------------
addProperty :: (Member (State (IM.IntMap a)) r, Typeable a) => ID -> a -> Eff r ()
addProperty eid val = modify $ IM.insert (unID eid) val

intersectionWith3 :: (a -> b -> c -> d)
                  -> Component a -> Component b -> Component c -> Component d
intersectionWith3 f a b c = IM.intersectionWith ($) (IM.intersectionWith f a b) c

intersectionWith4 :: (a -> b -> c -> d -> e)
                  -> Component a -> Component b -> Component c -> Component d
                  -> Component e
intersectionWith4 f a b c d = IM.intersectionWith ($) (intersectionWith3 f a b c) d

displayAll dt = do
    (colors     :: Component Colors)       <- get
    (positions  :: Component Position)     <- get
    (rotations  :: Component Rotation)     <- get
    (faces      :: Component Displayable)  <- get
    (bodies     :: Component PhysicalBody) <- get
    (offsets    :: Component PositionOffset) <- stepVaryingComponent dt runIdentity

    let bodyPositions = fmap (aabbPosition . pbAABB) bodies
        positions' = IM.unionWith (^+^) offsets $ IM.union bodyPositions positions

    window   <- ask >>= lift . getWindow
    renderer <- ask

    let display :: Colors -> Displayable -> Position -> Rotation -> IO ()
        display clrs face pos rot = (drawWith renderer) clrs face pos (V2 1 1) rot

    -- Run our entanglements?
    --entanglements <- get >>= \ets -> R.forM (IM.toList ets) $ uncurry runEntanglement

    lift $ do makeContextCurrent $ Just window
              (drawWith renderer) (Colors transparent transparent) CleanFrame zero zero 0
              -- Display all toons.
              let draws = IM.elems $ intersectionWith4 (,,,) colors faces positions' rotations
                  draws' = sortBy (\(_,_,p1,_) (_,_,p2,_) -> compare (p1^._y) (p2^._y)) draws
                  drawIOs = fmap (\(a,b,c,d) -> display a b c d) draws'
              sequence_ drawIOs
              --sequence_ entanglements
              let clrs = Colors transparent $ red `alpha` 0.2
              sequence_ $ fmap (\(PhysicalBody (AABB p hw hh) _) -> display clrs (Box hw hh) p 0)
                               bodies

              --mapM_ displayName namesOfToons
              swapBuffers window

loadNewEvents = do
    lift $ pollEvents
    events <- ask >>= lift . getNewEvents
    env    <- get
    let env'  = Prelude.foldl foldInput env events
    put env'


clearLastEvents = modify clearEvents


handleQuit = ask >>= lift . getWindow >>= \window -> lift $ do
    shouldClose <- windowShouldClose window
    R.when shouldClose exitSuccess

main :: IO ()
main = do
    wref     <- initWindow (V2 300 600) (V2 300 300) "ld31"
    renderer <- newRenderer =<< getWindow wref
    t        <- getCurrentTime

    runLift -- $ evalState (mempty :: Component (Varying (R.Reader InputEnv) Position))
            $ evalState (mempty :: Component Displayable)
            $ evalState (mempty :: Component Position)
            $ evalState (mempty :: Component Rotation)
            $ evalState (mempty :: Component Velocity)
            $ evalState (mempty :: Component Colors)
            $ evalState (mempty :: Component Name)
            $ evalState (mempty :: Component PhysicalBody)
            $ evalState (mempty :: Component (PlayerDirection Key))
            $ evalState (mempty :: Component (PlayerDirection Int))
            $ evalState (mempty :: Component (PlayerRotation Int))
            -- $ evalState (mempty :: Component Entanglement)
            $ evalState (mempty :: VaryingComponent Identity PositionOffset)
            $ evalState (Player $ ID 0)
            $ evalState emptyInputEnv
            $ evalState t
            $ flip runReader wref
            $ flip runReader renderer
            $ flip runFresh (ID 0) $ do
                initialize
                R.forever play
