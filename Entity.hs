{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Entity where

import Prelude hiding (foldl, mapM_, sequence_)
import Gelatin hiding (drawArrays, get, Position, renderer, varying, Name)
--import Linear
import Yarn
import Rendering
import Data.Time.Clock
import Data.Typeable
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import Control.Monad hiding (mapM_, sequence_)
import Control.Lens
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import System.Exit

deriving instance Typeable Identity

--------------------------------------------------------------------------------
-- Types for our entity component system.
--------------------------------------------------------------------------------
newtype ID = ID { unID :: Int } deriving (Show, Read, Eq, Ord, Typeable, Enum, Num)
type Component a = IM.IntMap a
type Entity a = State (Component a)
type CanRead a r = Member (Reader a) r
type CanModify a r = Member (State a) r
type Varying m a = Yarn m () a
--------------------------------------------------------------------------------
-- FRP types for things that vary over time and then some...
--------------------------------------------------------------------------------
--type TimeVarying a = Yarn Identity () a
--type InputVarying a = Yarn (Reader InputEnv) () a
--type IOVarying a = Yarn IO () a
--------------------------------------------------------------------------------
-- Components (not a complete list)
--------------------------------------------------------------------------------
newtype Position = Position (V2 Float) deriving (Show, Typeable)
newtype Velocity = Velocity (V2 Float) deriving (Show, Typeable)
newtype Name = Name String deriving (Show, Typeable)
--------------------------------------------------------------------------------
-- Context helpers
--------------------------------------------------------------------------------
unvelocity :: Velocity -> V2 Float
unvelocity (Velocity v) = v

unposition :: Position -> V2 Float
unposition (Position p) = p

incrementPosition :: Velocity -> Position -> Position
incrementPosition (Velocity v) (Position p) = Position $ v ^+^ p
--------------------------------------------------------------------------------
-- Progressing Components
--------------------------------------------------------------------------------
--progressVarying :: RealFloat t
--                => t -> Component (TimeVarying a)
--                -> (Component a, Component (TimeVarying a))
--progressVarying dt comps =
--    let stepMap = fmap (\y -> stepYarn y dt ()) comps
--        nextYarn (Identity (Output _ y)) = y
--        nextVal (Identity (Output v _)) = v
--        varying' = fmap nextYarn stepMap
--        static' = fmap nextVal stepMap
--    in (static', varying')
--
--progressComponent :: ( Member (Entity (TimeVarying a)) r
--                     , Member (Entity a) r
--                     , Typeable a
--                     , RealFloat t)
--                  => t -> Eff r (Component a)
--progressComponent dt = do
--    vars <- get
--    let (vals', vars') = progressVarying dt vars
--    put vars'
--
--    vals <- get
--    let vals'' = IM.union vals' vals
--    put vals''
--    return vals''
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

displayAll :: ( SetMember Lift (Lift IO)               r
              , Member (State UTCTime)                 r
              , Member (Entity Displayable)            r
              , Member (Entity Position)               r
              , Member (Entity Colors)                 r
              , Member (Entity Name)                   r
              , Member (Reader WindowRef)              r
              , Member (Reader Renderer)               r)
           => Eff r ()
displayAll = do
    (clrMap :: Component Colors)  <- get
    (posMap :: Component Position) <- get
    (faceMap :: Component Displayable) <- get
    (names :: Component Name)   <- get
    window   <- ask >>= lift . getWindow
    renderer <- ask

    let
        display :: Colors -> Displayable -> Position -> IO ()
        display clrs face (Position pos) = (drawWith renderer) clrs face pos (V2 1 1) 0

    lift $ do makeContextCurrent $ Just window
              (drawWith renderer) (Colors transparent transparent) CleanFrame zero zero 0
              -- Display all toons.
              sequence_ $ intersectionWith3 display clrMap faceMap posMap
              --mapM_ displayName namesOfToons
              swapBuffers window

loadNewEvents :: ( SetMember Lift (Lift IO)     r
                 , Member    (Reader WindowRef) r
                 , Member    (State InputEnv)   r)
              => Eff r ()
loadNewEvents = do
    lift $ pollEvents
    events <- ask >>= lift . getNewEvents
    env    <- get
    let env' = foldl foldInput env events
    lift $ print $ ienvKeysDown env'
    put env'

clearLastEvents :: ( SetMember Lift (Lift IO)     r
                   , Member    (State InputEnv)   r)
                => Eff r ()
clearLastEvents = modify clearEvents

handleQuit :: ( SetMember Lift (Lift IO)     r
              , Member    (Reader WindowRef) r)
           => Eff r ()
handleQuit = ask >>= lift . getWindow >>= \window -> lift $ do
    shouldClose <- windowShouldClose window
    when shouldClose exitSuccess
