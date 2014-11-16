{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding (foldl)
import Gelatin hiding (drawArrays, get, Pos)
import Graphics.Rendering.OpenGL hiding (ortho, triangulate, translate, scale, rotate, get)
import Yarn
import Shaders
import Data.IORef
import Data.Time.Clock
import Data.Maybe
import Data.Typeable
import Data.Foldable
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Control.Lens
import Control.Monad
--import Control.Monad.Reader
--import Control.Monad.State
import Control.Concurrent
import Control.Applicative
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.State.Strict
import System.Exit

data Frame = Frame

newtype ID = ID { unID :: Int } deriving (Show, Read, Eq, Ord, Typeable, Enum, Num)

data Displayable = CleanFrame
                 | FullSheet
                 | Reaper
                 | Text String

type Renderer = Colors -> Displayable -> RenderSprite

data Colors = Colors { foregroundColor :: V4 Float
                     , backgroundColor :: V4 Float
                     } deriving (Typeable)
newtype Pos = Pos (V2 Float) deriving (Typeable)

data Entities = Entities { positionEnts :: IM.IntMap Pos
                         , colorsEnts   :: IM.IntMap Colors
                         , nextEntityID :: ID
                         }

instance Monoid Entities where
    mempty = Entities mempty mempty (ID 0)
    (Entities ps clrs nid) `mappend` (Entities ps' clrs' nid') =
        Entities (ps `mappend` ps') (clrs `mappend` clrs') (max nid nid')

type RenderSprite = V2 Float -> V2 Float -> Float -> IO ()

data SpriteSheet = SpriteSheet { ssObject :: TextureObject
                               , ssWidth  :: Int
                               , ssHeight :: Int
                               , ssName   :: String
                               }

--type Env = Reader InputEnv
type Network = Yarn Identity () Frame

data Game = Game { windowRef    :: WindowRef
                 , draw         :: Renderer
                 , env          :: InputEnv
                 , lastUTCTime  :: UTCTime
                 , network      :: Network
                 , entities     :: Entities
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

    draws <- forM kvs $ \(c, (v, w, h)) -> do
        r <- newSpriteRenderer s ss v w h
        return (c,r)

    let m = M.fromList draws
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
    drawText      <- newTextRenderer s ss

    clearColor $= toColor4 (black :: V4 Float)
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))

    currentProgram $= (Just $ program s)
    updateUniform s $ uniformV4fv "texColor" ([white, black] :: [V4 Float])

    let setColors (Colors fg bg) = updateUniform s $
                                       uniformV4fv "replaceColor" [fg, bg]

    return $ \clrs thing pos scl rot -> setColors clrs >> case thing of
        CleanFrame -> do
            clear [ColorBuffer]
            (w,h) <- getWindowSize window
            let [w',h'] = map fromIntegral [w,h] :: [Float]
                [w'',h''] = map ((*2) . fromIntegral) [w,h] :: [GLint]

            viewport $= (Position 0 0, Size w'' h'')
            clear [ColorBuffer]
            updateUniform s $ uniformi "sampler" (0 :: Int)
            updateUniform s $ uniformM4f "projection" $ ortho 0 w' 0 h' 0 1

        FullSheet -> drawFullSheet pos scl rot
        Reaper    -> drawReaper pos scl rot
        Text str  -> drawText str pos scl rot

newGame :: IO Game
newGame = do
    wref <- initWindow (V2 300 600) (V2 600 600) "ludum-helpers"
    t    <- getCurrentTime
    r    <- readIORef wref >>= newRenderer . snd
    return $ Game wref r emptyInputEnv t (pure Frame) mempty

displayAll :: ( SetMember Lift (Lift IO)          r
              , Member    (State (IM.IntMap Colors)) r
              , Member    (State (IM.IntMap Pos)) r
              ) => Renderer -> Eff r ()
displayAll draw = do
    let display _ clrs (Pos pos) = draw clrs Reaper pos (V2 1 1) 0
    fMap <- IM.intersectionWithKey display <$> get <*> get
    lift $ sequenceA_ fMap

--runEntities :: (Typeable m, Monad m)
--            => Eff (Fresh ID :> (State (IM.IntMap Colors) :> (State (IM.IntMap Pos) :> (Lift m :> ())))) w
--            -> Entities -> m w
runEntities eff Entities{..} = do
    entities' <- runLift $ runState positionEnts $ runState colorsEnts $
                     runFresh eff nextEntityID
    return entities


loop :: Game -> IO ()
loop game = do
    (gameFrame, game'@Game{..}) <- stepGame game
    window <- snd <$> readIORef windowRef
    makeContextCurrent $ Just window

    -- Do our rendering
    draw (Colors transparent transparent) CleanFrame zero zero 0
    draw (Colors green transparent) Reaper (V2 0 0) (V2 1 1) 0

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
        Identity (Output gameFrame network') = stepYarn network dt ()

    -- Run our entities
--    entities' <- runEntities $ return ()

    return (gameFrame, g{ env = clearEvents env'
                        , lastUTCTime = t
                        , network = network'
                        })

main :: IO ()
main = do
    putStrLn "..."
    game <- newGame
    loop game
