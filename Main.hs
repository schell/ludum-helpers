{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Gelatin hiding (drawArrays, renderer)
import Graphics.Rendering.OpenGL hiding (ortho, triangulate, renderer, translate, scale, rotate)
import Yarn
import Shaders
import Data.IORef
import Data.Time.Clock
import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Applicative
import System.Exit

data Frame = Frame
type Renderer = Frame -> IO ()
type RenderSprite = V2 Float -> V2 Float -> Float -> IO ()
data SpriteSheet = SpriteSheet { ssObject :: TextureObject
                               , ssWidth  :: Int
                               , ssHeight :: Int
                               , ssName   :: String
                               }
type Env = Reader InputEnv
type Network = Yarn Env () Frame
data Game = Game { windowRef    :: WindowRef
                 , renderer     :: Renderer
                 , env          :: InputEnv
                 , lastUTCTime  :: UTCTime
                 , network      :: Network
                 }

loadSpriteSheet :: String -> Int -> Int -> IO SpriteSheet
loadSpriteSheet fp w h = do
    t <- fromJust . M.lookup fp <$>
             loadTextureSrc (Relative fp)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    return $ SpriteSheet t w h fp

-- A map of chars to their bitmap position and size in our spritesheet.
charMetrics :: M.Map Char (V2 Int, Int, Int)
charMetrics =
    M.fromList [ (' ', (V2 0 0, 7, 7))
               , ('0', (V2 1  1079, 6,7))
               , ('1', (V2 6  1079, 4,7))
               , ('2', (V2 9  1079, 6,7))
               , ('3', (V2 14 1079, 6,7))
               ]

newCharRenderer :: ShaderProgram -> SpriteSheet -> IO (Char -> RenderSprite)
newCharRenderer s ss = do
    let cm = charMetrics
        keys = M.keys cm
        vals = catMaybes $ map (`M.lookup` cm) keys
        kvs = zip keys vals

    renderers <- forM kvs $ \(c, (v, w, h)) -> do
        r <- newSpriteRenderer s ss v w h
        return (c,r)

    let m = M.fromList renderers
    return $ \c vp vs r -> case M.lookup c m of
                               Nothing -> putStrLn $ c : " has not been loaded."
                               Just f  -> f vp vs r

newTextRenderer :: ShaderProgram -> SpriteSheet -> IO (String -> RenderSprite)
newTextRenderer s ss = do
    cr <- newCharRenderer s ss
    let cm = charMetrics
        f [] _ _ _ = return ()
        f (c:str) vp vs r = case M.lookup c cm of
                                Nothing -> f (' ':str) vp vs r
                                Just (V2 _ _, w, _) -> do cr c vp vs r
                                                          f str (vp + vs * V2 (fromIntegral w) 0) vs r

    return f

newSpriteRenderer :: ShaderProgram -> SpriteSheet -> V2 Int -> Int -> Int -> IO RenderSprite
newSpriteRenderer s ss p w h = do
    let ps = fmap (fmap fromIntegral) [V2 0 0, V2 w 0, V2 w h, V2 0 h] :: [V2 Float]
        [ssw,ssh] = fmap fromIntegral [ssWidth ss, ssHeight ss]
        [w',h'] = fmap fromIntegral [w, h]
        -- Origin of the rectangle in u,v coords.
        (V2 ox oy) = fmap fromIntegral p & _x %~ (/ ssw) & _y %~ (/ ssh)
        (V2 cx cy) = V2 ox oy ^+^ V2 (w'/ssw) (h'/ssh)
        ts     = [V2 ox oy, V2 cx oy, V2 cx cy, V2 ox cy] :: [V2 Float]
        t      = ssObject ss
    vbov <- bufferVBO s (position2 ps)
    vbot <- bufferVBO s (texcoord ts)

    return $ \vp vs r -> usingShader s $ withTextures2D [t] $ withVBOs s [vbov,vbot] $ do
        let mv = mkM44 $ do translate $ embed vp
                            scale $ embedWith vs 1
                            rotate r (V3 0 0 1)
        updateUniform s $ uniformi "sampler" (0 :: Int)
        updateUniform s $ uniformM4f "modelview" (mv :: M44 Float)

        drawArrays TriangleFan 0 4

newRenderer :: Window -> IO Renderer
newRenderer window = do
    --s  <- simple2dTextureShader
    s  <- colorReplaceTextureShader
    ss <- loadSpriteSheet "img/oryx_roguelike_16x24.png" 304 1184

    drawFullSheet <- newSpriteRenderer s ss (V2 0 0) 304 1184
    drawReaper    <- newSpriteRenderer s ss (V2 135 1070) 42 53
    drawChar      <- newCharRenderer s ss
    drawText      <- newTextRenderer s ss

    clearColor $= toColor4 (black :: V4 Float)
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))

    currentProgram $= (Just $ program s)
    updateUniform s $ uniformV4fv "texColor" ([black, white] :: [V4 Float])
    updateUniform s $ uniformV4fv "replaceColor" ([blue, red] :: [V4 Float])

    return $ const $ do
        currentProgram $= (Just $ program s)
        clear [ColorBuffer]
        (w,h) <- getWindowSize window
        let [w',h'] = map fromIntegral [w,h] :: [Float]
            [w'',h''] = map ((*2) . fromIntegral) [w,h] :: [GLint]

        viewport $= (Position 0 0, Size w'' h'')
        clear [ColorBuffer]
        updateUniform s $ uniformi "sampler" (0 :: Int)
        updateUniform s $ uniformM4f "projection" $ ortho 0 w' 0 h' 0 1

        drawFullSheet (V2 100 100) (V2 1 1) 0
        drawReaper (V2 10 20) (V2 2 2) (pi/4)
        drawChar '0' (V2 20 20) (V2 1 1) 0
        drawText "0123 0123" (V2 23 20) (V2 4 4) 0

newGame :: IO Game
newGame = do
    wref <- initWindow (V2 300 600) (V2 600 600) "ludum-helpers"
    t    <- getCurrentTime
    r    <- readIORef wref >>= newRenderer . snd
    return $ Game wref r emptyInputEnv t $ pure Frame

loop :: Game -> IO ()
loop game = do
    (gameFrame, game'@Game{..}) <- stepGame game
    window <- snd <$> readIORef windowRef
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
