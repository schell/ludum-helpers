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
import Types
import Toons
import UserInput
import Rendering
import Data.Time.Clock
import Data.Typeable
import Data.Monoid
import Data.Foldable
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Control.Monad.Reader as R
import Control.Concurrent
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Control.Applicative
import System.Exit

deriving instance Typeable (R.ReaderT)
data Entanglement = ProjectsOntoAxis ID
                  | CanCollideWith ID
                  deriving Typeable


initialize = do
    axis <- fresh
    axis `addProperty` (Box 1000 1)
    axis `addProperty` Colors transparent white
    axis `addProperty` (0 :: Rotation)
    axis `addProperty` (V2 300 300 :: Position)
    axis `addProperty` (playerRotation bumperRotationMap)

    reaper <- fresh
    reaper `addProperty` (Reaper South)
    reaper `addProperty` (V2 100 100 :: Position)
    reaper `addProperty` (0 :: Rotation)
    reaper `addProperty` Colors green transparent
    reaper `addProperty` (playerDirection wasdDirectionMap)
    reaper `addProperty` (AABB 0 50 50)

    pinky <- fresh
    pinky `addProperty` (Reaper East)
    pinky `addProperty` (V2 0 0 :: Position)
    pinky `addProperty` (0 :: Rotation)
    pinky `addProperty` Colors pink transparent
    pinky `addProperty` (playerDirection dpadDirectionMap)
    pinky `addProperty` (AABB 0 50 50)
    pinky `addProperty` (CanCollideWith reaper)


    -- How to entangle two entities so they can interact with each other in
    -- a special way? Maybe I'd have to enumerate all possible interactions
    -- using a type and then in `play` I could map the type to a function
    -- to perform.

-- Find entities that have keyboard control and progress their positions.
stepKeyboardControlledPositions dt = do
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
    lift $ print buttonSet
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

play = do
    -- Tick time.
    t' <- lift $ getCurrentTime
    t  <- get
    put t'
    let dt = realToFrac $ diffUTCTime t' t :: Float

    -- Get the user events and fold them into our InputEnv.
    loadNewEvents

    stepKeyboardControlledPositions dt
    stepJoystickControlledThings dt

    -- Find things that have an aabb and a position. Update the aabb's
    -- position.
    positions <- get
    aabbs     <- get
    let aabbs' = IM.intersectionWith (\p (AABB _ w h) -> AABB p w h)
                                     positions
                                     aabbs
    modify $ IM.union aabbs'

    -- Use our velocities to set some displayables
    vvals <- get
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

aabbAxes :: [SeparatingAxis]
aabbAxes =
    [ V2 1    0
    , V2 0    1
    ]

aabbLines :: AABB -> [Line]
aabbLines a = [(tl, tr), (tr, br), (br, bl), (bl, tl)]
    where [tl,tr,bl,br] = aabbPoints a

aabbPoints :: AABB -> [V2 Float]
aabbPoints (AABB (V2 x y) hw hh) = [V2 l t, V2 r t, V2 l b, V2 r b]
    where (l,t,r,b) = (x - hw, y - hh, x + hw, y + hh)

aabbProjectionRange :: AABB -> V2 Float -> (Float, Float)
aabbProjectionRange aabb axis = range
    where range = Prelude.foldl (\(n,x) p' -> (min n p', max x p'))
                                (1/0, -(1/0))
                                prjs
          ps    = aabbPoints aabb
          prjs  = map (`dot` axis) ps

collidesWithRange :: (Float, Float) -> (Float, Float) -> Bool
collidesWithRange (a,b) (c,d) = not (d <= a || b <= c)

collideOnAxis :: AABB -> AABB -> V2 Float -> Maybe (V2 Float)
collideOnAxis a b axis = if col then Just $ u ^* v else Nothing
    where rA@(n1, n2) = aabbProjectionRange a axis
          rB@(m1, m2) = aabbProjectionRange b axis
          v = min (m2 - n1) (n2 - m1)
          u = signorm axis
          col = collidesWithRange rA rB

collidedInto :: AABB -> AABB -> Maybe (V2 Float)
collidedInto a b = (minimumBy dv) <$> (sequence axisOverlaps)
    where axisOverlaps = map (collideOnAxis a b) aabbAxes
          dv v1 v2 = compare (norm v1) (norm v2)

runEntanglement thing (ProjectsOntoAxis (ID axis)) = do
    rotations <- get
    aabbs     <- get
    renderer  <- ask

    return $ maybe (return ()) id $ do
        (theta :: Rotation) <- IM.lookup axis rotations
        aabb@(AABB p _ _) <- IM.lookup thing aabbs
        let vaxis   = V2 (cos theta) (sin theta)
            (mn,mx) = aabbProjectionRange aabb vaxis

        return $ (drawWith renderer) (Colors transparent white)
                                     (Box (mx - mn) 1)
                                     p
                                     (V2 1 1)
                                     theta
runEntanglement a (CanCollideWith (ID b)) = do
    aabbs <- get
    return $ maybe (return ()) id $ do
        aabba <- IM.lookup a aabbs
        aabbb <- IM.lookup b aabbs
        return $ putStrLn $ unwords [show a, "collided into", show b, "is"
                                    , show $ aabba `collidedInto` aabbb
                                    ]


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

displayAll = do
    (colors     :: Component Colors)      <- get
    (positions  :: Component Position)    <- get
    (rotations  :: Component Rotation)    <- get
    (faces      :: Component Displayable) <- get
    (aabbs      :: Component AABB) <- get

    window   <- ask >>= lift . getWindow
    renderer <- ask

    let display :: Colors -> Displayable -> Position -> Rotation -> IO ()
        display clrs face pos rot = (drawWith renderer) clrs face pos (V2 1 1) rot

    -- Run our entanglements?
    entanglements <- get >>= \ets -> R.forM (IM.toList ets) $ uncurry runEntanglement

    lift $ do makeContextCurrent $ Just window
              (drawWith renderer) (Colors transparent transparent) CleanFrame zero zero 0
              -- Display aabbs for testing.
              forM_ (IM.elems aabbs) $ \(AABB p hw hh) -> do
                  let (w,h) = (2*hw, 2*hh)
                  (drawWith renderer) (Colors transparent $ red `alpha` 0.5) (Box w h) p (V2 1 1) 0
              -- Display all toons.
              sequence_ $ intersectionWith4 display colors faces positions rotations
              sequence_ entanglements
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
    wref     <- initWindow (V2 300 600) (V2 600 600) "ludum-helpers"
    renderer <- newRenderer =<< getWindow wref
    t        <- getCurrentTime

    runLift -- $ evalState (mempty :: Component (Varying (R.Reader InputEnv) Position))
            $ evalState (mempty :: Component Displayable)
            $ evalState (mempty :: Component Position)
            $ evalState (mempty :: Component Rotation)
            $ evalState (mempty :: Component Velocity)
            $ evalState (mempty :: Component Colors)
            $ evalState (mempty :: Component Name)
            $ evalState (mempty :: Component AABB)
            $ evalState (mempty :: Component (PlayerDirection Key))
            $ evalState (mempty :: Component (PlayerDirection Int))
            $ evalState (mempty :: Component (PlayerRotation Int))
            $ evalState (mempty :: Component Entanglement)
            $ evalState emptyInputEnv
            $ evalState t
            $ flip runReader wref
            $ flip runReader renderer
            $ flip runFresh (ID 0) $ do
                initialize
                R.forever play
