{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Rendering where

import Toons
import Gelatin hiding (drawArrays, get, Position)
import Graphics.Rendering.OpenGL hiding (ortho, triangulate, translate, scale, rotate, get, Position)
import qualified Graphics.Rendering.OpenGL as GL
import Shaders
import Data.Maybe
import Data.Typeable
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad hiding (mapM_, sequence_)
import Control.Applicative

type RenderFunc = Colors -> Displayable -> RenderSprite
newtype Renderer = Renderer RenderFunc deriving (Typeable)

drawWith :: Renderer -> RenderFunc
drawWith (Renderer f) = f

data Colors = Colors { foregroundColor :: V4 Float
                     , backgroundColor :: V4 Float
                     } deriving (Typeable, Show)

type RenderSprite = V2 Float -> V2 Float -> Float -> IO ()

data SpriteSheet = SpriteSheet { ssObject :: TextureObject
                               , ssWidth  :: Int
                               , ssHeight :: Int
                               , ssName   :: String
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
charMetrics = M.fromList $ concat $ zipWith mk [row1,row2,row3] [V2 9 1130, V2 9 1138, V2 9 1146]
    where row1    = [' ' .. '@']
          row2    = ['A' .. '`']
          row3    = ['a' .. '~']
          mk :: String -> V2 Int -> [(Char, (V2 Int, Int, Int))]
          mk row p = zip row $ zip3 (length row `pointsFrom` p) (cycle [5]) (cycle [7])
          pointsFrom n p = zipWith (\n' p' -> p' ^+^ V2 (n'*6) 0) [0 .. n] $ cycle [p]

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
    return $ renderText cr

renderText :: (Char -> RenderSprite) -> String -> RenderSprite
renderText render str pos scl rot = renderText' str pos
    where renderText' []       _    = return ()
          renderText' ('\n':str') _ = renderText render str' (pos + scl * V2 0 7) scl rot
          renderText' (c:str') pos' =
              case M.lookup c charMetrics of
                  Nothing             -> renderText' (' ':str') pos'
                  Just (V2 _ _, w, _) -> do render c pos' scl rot
                                            renderText' str' (pos' + scl * V2 (fromIntegral w) 0)

newSpriteRenderer :: ShaderProgram -> SpriteSheet -> V2 Int -> Int -> Int -> IO RenderSprite
newSpriteRenderer s ss p w h = do
    let (hw, hh) = ((fromIntegral w)/2, (fromIntegral h)/2)
        ps = [V2 (-hw) (-hh), V2 hw (-hh), V2 hw hh, V2 (-hw) hh] :: [V2 Float]
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
                            rotate r (V3 0 0 1)
                            scale $ embedWith vs 1
        updateUniform s $ uniformi "sampler" (0 :: Int)
        updateUniform s $ uniformM4f "modelview" (mv :: M44 Float)
        drawArrays TriangleFan 0 4

newReaperRenderer :: ShaderProgram -> SpriteSheet -> IO (CardinalDirection -> RenderSprite)
newReaperRenderer s ss = do
    south <- newSpriteRenderer s ss (V2 0 1077) 40 51
    east  <- newSpriteRenderer s ss (V2 42 1077) 40 51
    west  <- newSpriteRenderer s ss (V2 83 1077) 40 51
    north <- newSpriteRenderer s ss (V2 125 1077) 40 51

    return $ \d pos scl rot -> case d of
                                   South -> south pos scl rot
                                   East  -> east pos scl rot
                                   West  -> west pos scl rot
                                   North -> north pos scl rot

newBoxRenderer :: ShaderProgram -> IO RenderSprite
newBoxRenderer s = do
    let ps = [V2 (-0.5) (-0.5), V2 0.5 (-0.5), V2 0.5 0.5, V2 (-0.5) 0.5]
        ts = [V2 0 0, V2 1 0, V2 1 1, V2 0 1]

    vbo <- bufferVBO s (position2 ps)
    tvbo <- bufferVBO s (texcoord ts)

    return $ \vp vs r -> usingShader s $ withVBOs s [vbo, tvbo] $ do
        let mv = mkM44 $ do translate $ embed vp
                            rotate r (V3 0 0 1)
                            scale $ embedWith vs 1
        updateUniform s $ uniformi "sampler" (0 :: Int)
        updateUniform s $ uniformM4f "modelview" (mv :: M44 Float)
        drawArrays TriangleFan 0 4

newRenderer :: Window -> IO Renderer
newRenderer window = do
    --s  <- simple2dTextureShader
    s  <- colorReplaceTextureShader
    ss <- loadSpriteSheet "img/oryx_roguelike_16x24.png" 304 1184

    drawFullSheet <- newSpriteRenderer s ss (V2 0 0) 304 1184
    drawReaper    <- newReaperRenderer s ss
    drawText      <- newTextRenderer s ss
    drawBox       <- newBoxRenderer s

    currentProgram $= (Just $ program s)
    clearColor $= toColor4 (black :: V4 Float)
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))

    return $ Renderer $ \(Colors fg bg) thing pos scl rot -> do
        currentProgram $= (Just $ program s)
        updateUniform s $ uniformV4fv "texColor" ([white, black] :: [V4 Float])
        updateUniform s $ uniformV4fv "replaceColor" [fg, bg]

        case thing of
            CleanFrame -> do
                clear [ColorBuffer]
                (w,h) <- getWindowSize window
                let [w',h'] = map fromIntegral [w,h] :: [Float]
                    [w'',h''] = map ((*2) . fromIntegral) [w,h] :: [GLint]

                viewport $= (GL.Position 0 0, Size w'' h'')
                clear [ColorBuffer]
                updateUniform s $ uniformi "sampler" (0 :: Int)
                updateUniform s $ uniformM4f "projection" $ ortho 0 w' 0 h' 0 1

            FullSheet -> drawFullSheet pos scl rot
            Reaper d  -> drawReaper d pos scl rot
            Text str  -> drawText str pos scl rot
            Box hw hh -> drawBox pos (V2 (2*hw) (2*hh) * scl) rot
