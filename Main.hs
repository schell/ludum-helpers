{-# LANGUAGE RecordWildCards #-}
module Main where

--import Gelatin hiding (renderer)
import Yarn
import Data.IORef
import Data.Time.Clock
import Data.Maybe
import Data.Map.Strict as M hiding (foldl)
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Applicative
import System.Exit

data Frame = Frame
type Renderer = Frame -> IO ()
type Env = Reader InputEnv
type Network = Yarn Env () Frame
data Game = Game { windowRef    :: WindowRef
                 , renderer     :: Renderer
                 , env          :: InputEnv
                 , lastUTCTime  :: UTCTime
                 , network      :: Network
                 }

newRenderer :: IO Renderer
newRenderer = do
    t <- fromJust . M.lookup "img/oryx_roguelike_16x24.png" <$>
             loadTextureSrc (Relative "img/oryx_roguelike_16x24.png")
    textureFilter Texture2D $= (Nearest, Nothing) Nearest
    textureWrapMode S Repeated Clamp
    return $ const $ withTextures2D [t] $ do


newGame :: IO Game
newGame = do
    wref <- initWindow (V2 300 600) (V2 600 600) "ludum-helpers"
    t    <- getCurrentTime
    r    <- newRenderer
    return $ Game wref r emptyInputEnv t $ pure Frame

loop :: Game -> IO ()
loop game = do
    (gameFrame, game'@Game{..}) <- stepGame game
    window <- snd <$> readIORef windowRef
    (w,h)  <- getWindowSize window
    makeContextCurrent $ Just window

    -- Do our rendering

    renderer gameFrame

    -- Handle quitting
    swapBuffers window
    shouldClose <- windowShouldClose window
    when shouldClose exitSuccess
    threadDelay 100
    loop game'

stepGame :: Game -> IO (Frame, Game)
stepGame g@Game{..} = do
    pollEvents
    (events, window) <- readIORef windowRef
    let env' = foldl foldInput env events
    writeIORef windowRef ([], window)

    -- Pull the game data down out of the network
    t <- getCurrentTime
    let dt = realToFrac $ diffUTCTime t lastUTCTime
        Output gameFrame network' = runReader (stepYarn network dt ()) env'

    return (gameFrame, g{ env = clearEvents env'
                       , lastUTCTime = t
                       , network = network'
                       })

main :: IO ()
main = do
    putStrLn "..."
    game <- newGame
    loop game
