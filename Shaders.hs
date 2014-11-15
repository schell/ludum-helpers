module Shaders where

import           Graphics.Rendering.OpenGL hiding (Color, color)
import           Graphics.GLUtil
import qualified Data.ByteString.Char8 as BS

shaderVertSrc :: BS.ByteString
shaderVertSrc = BS.pack $ unlines
    [ "uniform mat4 projection;"
    , "uniform mat4 modelview;"
    , "attribute vec2 position;"
    , "attribute vec2 texcoord;"
    , "varying vec2 ftexcoord;"
    , "void main(void) {"
    , " ftexcoord = texcoord;"
    , " gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
    , "}"
    ]

texReplaceFragSrc :: BS.ByteString
texReplaceFragSrc = BS.pack $ unlines
    [ "uniform sampler2D sampler;"
    , "// the colors to replace"
    , "uniform vec4 texColor[2];"
    , "// the colors to replace with"
    , "uniform vec4 replaceColor[2];"
    , "varying vec2 ftexcoord;"

    , "bool withinRange(float r, vec4 v1, vec4 v2) {"
    , "  if (abs(v1.r - v2.r) > r) {"
    , "    return false;"
    , "  }"
    , "  if (abs(v1.g - v2.g) > r) {"
    , "    return false;"
    , "  }"
    , "  if (abs(v1.b - v2.b) > r) {"
    , "    return false;"
    , "  }"
    , "  if (abs(v1.a - v2.a) > r) {"
    , "    return false;"
    , "  }"
    , " return true;"
    , "}"

    , "void main(void) {"
    , "  vec4 color = texture2D(sampler, ftexcoord);"
    , "  for(int i = 0; i < 2; i++) {"
    , "    vec4 tc = texColor[i];"
    , "    vec4 rc = replaceColor[i];"

    , "    if (withinRange(0.01, tc, color)) {"
    , "        gl_FragColor = rc;"
    , "        return;"
    , "    }"
    , "  }"
    , "  gl_FragColor = color;"
    , "}"

    ]

colorReplaceTextureShader :: IO ShaderProgram
colorReplaceTextureShader =
    loadShaderProgramBS [ (VertexShader, shaderVertSrc)
                        , (FragmentShader, texReplaceFragSrc)
                        ]
