{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Control.Monad hiding (mapM_)
import Control.Lens
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import System.Exit

deriving instance Typeable Identity
deriving instance Typeable Yarn

--------------------------------------------------------------------------------
-- General Types
--------------------------------------------------------------------------------
newtype ID = ID { unID :: Int } deriving (Show, Read, Eq, Ord, Typeable, Enum, Num)

type TimeVarying a = Yarn Identity () a

type Component a = IM.IntMap a
type Entity a = State (Component a)
--------------------------------------------------------------------------------
-- Components
--------------------------------------------------------------------------------
type Position = V2 Float
newtype Name = Name String deriving Typeable
--------------------------------------------------------------------------------
-- Progressing Components
--------------------------------------------------------------------------------
progressVarying :: RealFloat t
                => t -> Component (TimeVarying a)
                -> (Component a, Component (TimeVarying a))
progressVarying dt comps =
    let stepMap = fmap (\y -> stepYarn y dt ()) comps
        nextYarn (Identity (Output _ y)) = y
        nextVal (Identity (Output v _)) = v
        varying' = fmap nextYarn stepMap
        static' = fmap nextVal stepMap
    in (static', varying')

progressComponent :: ( Member (State (Component (TimeVarying a))) r
                     , Member (State (Component a)) r
                     , Typeable a
                     , RealFloat t)
                  => t -> Eff r (Component a)
progressComponent dt = do
    vars <- get
    let (vals', vars') = progressVarying dt vars
    put vars'

    vals <- get
    let vals'' = IM.union vals' vals
    put vals''
    return vals''
--------------------------------------------------------------------------------
-- Entities
--------------------------------------------------------------------------------
addProperty :: (Member (State (IM.IntMap a)) r, Typeable a) => ID -> a -> Eff r ()
addProperty eid val = modify $ IM.insert (unID eid) val

displayAll :: ( SetMember Lift (Lift IO)               r
              , Member (State UTCTime)                 r
              , Member (Entity (TimeVarying Colors))   r
              , Member (Entity (TimeVarying Position)) r
              , Member (Entity Position)               r
              , Member (Entity Colors)                 r
              , Member (Entity Name)                   r
              , Member (Reader WindowRef)              r
              , Member (Reader Renderer)               r)
           => Eff r ()
displayAll = do
    t' <- lift $ getCurrentTime
    t  <- get
    put t'
    let dt = realToFrac $ diffUTCTime t' t :: Double

    clrMap <- progressComponent dt
    posMap <- progressComponent dt
    names  <- get

    window   <- ask >>= lift . getWindow
    renderer <- ask

    let displayToon (clrs, pos) = (drawWith renderer) clrs Reaper pos (V2 1 1) 0
        displayName (Name n, (clrs, pos)) = do
            print n
            (drawWith renderer) clrs (Text n) pos (V2 2 2) 0
        toons = IM.intersectionWith (,) clrMap posMap
        namesOfToons = IM.intersectionWith (,) names toons
    lift $ do makeContextCurrent $ Just window
              (drawWith renderer) (Colors transparent transparent) CleanFrame zero zero 0
              mapM_ displayToon toons
              mapM_ displayName namesOfToons
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
