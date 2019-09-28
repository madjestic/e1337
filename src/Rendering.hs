{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Rendering
  ( openWindow
  , closeWindow
  , draw
  , initBufferObjects
  , initUniforms
  , Descriptor(..)
  , Drawable(..)
  ) where

import Control.Monad
import Data.Text                              (Text)
import Foreign.C
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
import Foreign.Storable                       (sizeOf)
import Graphics.Rendering.OpenGL as GL hiding (positions, color, normal, Size)
import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot)
import Data.Function                          (on)
import Graphics.GLUtil                        (readTexture, texture2DWrap)

import LoadShaders
import Game
import Object    as O
import Camera    as C
import Controllable
import Geometry
import Drawable
import Shape2D
import Material

import Data.Foldable     as DF (toList)
import Linear.Projection as LP (perspective)

import Unsafe.Coerce
import Debug.Trace as DT

openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"
         
    let config = OpenGLConfig { glColorPrecision = V4 8 8 8 0
                              , glDepthPrecision = 24
                              , glStencilPrecision = 8
                              , glMultisampleSamples = 8
                              , glProfile = Compatibility Normal 2 1}

    depthFunc $= Just Less

    window <- SDL.createWindow
            "e1337"
            SDL.defaultWindow { SDL.windowInitialSize = V2 sizex sizey
                              , SDL.windowOpenGL      = Just config }

    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window = do
    SDL.destroyWindow window
    SDL.quit

-- < OpenGL > -------------------------------------------------------------
data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices

draw :: SDL.Window -> Descriptor -> IO ()
draw window (Descriptor vao numIndices) =
  do
    GL.clearColor $= Color4 0.5 0 0 1
    GL.clear [ColorBuffer, DepthBuffer]
    bindVertexArrayObject $= Just vao
    drawElements Triangles numIndices GL.UnsignedInt nullPtr
    GL.pointSize $= 10

    cullFace  $= Just Back
    depthFunc $= Just Less

    SDL.glSwapWindow window

    
initUniforms :: Game -> IO ()
initUniforms game =  
  do
    -- | Shaders
    program <- loadShaders [
      ShaderInfo VertexShader
        (FileSource (vertShader . material . object $ game)),
      ShaderInfo FragmentShader
        (FileSource (fragShader . material . object $ game))
      ]
    currentProgram $= Just program

    -- | Set Uniforms
    location0         <- get (uniformLocation program "u_mouse")
    let u_mouse       = Vector2 (realToFrac $ fst mpos) (realToFrac $ snd mpos) :: Vector2 GLfloat
           where mpos = 
                   (pos . mouse . devices . C.controller . camera $ game)
    uniform location0 $= u_mouse
    
    location1         <- get (uniformLocation program "u_resolution")
    let u_res         = Vector2 (toEnum resX) (toEnum resY) :: Vector2 GLfloat
           where resX = fromEnum $ fst (resolution . options $ game)
                 resY = fromEnum $ snd (resolution . options $ game)
    uniform location1 $= u_res

    ticks             <- SDL.ticks
    let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float
    location2         <- get (uniformLocation program "u_time")
    uniform location2 $= (currentTime :: GLfloat)
    
    let proj =          
          fmap realToFrac . concat $ fmap DF.toList . DF.toList -- convert to GLfloat
          --               FOV    Aspect    Near   Far
          $ LP.perspective (pi/2) (resX/resY) (0.01) 1.5 :: [GLfloat]
                     where resX = toEnum $ fromEnum $ fst (resolution . options $ game)
                           resY = toEnum $ fromEnum $ snd (resolution . options $ game)

    persp             <- GL.newMatrix RowMajor proj :: IO (GLmatrix GLfloat)
    location3         <- get (uniformLocation program "persp")
    uniform location3 $= persp
    
    let cam =
          fmap realToFrac . concat $ fmap DF.toList . DF.toList $
          transform . C.controller . camera $ game --(identity::M44 Double) :: [GLfloat]
    camera            <- GL.newMatrix RowMajor cam :: IO (GLmatrix GLfloat)
    location4         <- get (uniformLocation program "camera")
    uniform location4 $= camera

    let mtx =
          fmap realToFrac . concat $ fmap DF.toList . DF.toList $
          (identity::M44 Double) :: [GLfloat]
    --_ <- DT.trace ("mtx: " ++ show mtx) $ return ()
    transform         <- GL.newMatrix RowMajor mtx :: IO (GLmatrix GLfloat)
    location5         <- get (uniformLocation program "transform")
    uniform location5 $= transform
    
    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing

    return () -- $ Descriptor vao (fromIntegral numIndices)    

instance ToDrawable Geo where
  toDrawable =
    (\x -> case x of
             Geo indices alpha color normal uv positions
               -> fromGeo (Geo indices alpha color normal uv positions)
             GLGeo vs idx
               -> return $ Drawable vs is'
               where
                 is'  = (map fromIntegral idx) :: [GLuint])

initBufferObjects :: Game -> IO Descriptor
initBufferObjects game =  
  do
    (Drawable vs idx) <- toDrawable $ (geometry . object) game

    -- | VAO
    vao <- genObjectName
    bindVertexArrayObject $= Just vao 

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral ((length vs) * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length idx
    withArray idx $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * sizeOf (head idx))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     =  14 * floatSize -- TODO : stride value should come from a single location

    -- | Colors
    let cds       = AttribLocation 0
        cdsOffset = 1 * floatSize
    vertexAttribPointer cds  $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset cdsOffset))
    vertexAttribArray cds    $= Enabled
    
    -- | UV
    let uvCoords   = AttribLocation 1
        uvOffset   = 7 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled
    
    -- | Positions
    let vPosition  = AttribLocation 2
        posOffset  = 10 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 4 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | Assign Textures
    activeTexture            $= TextureUnit 0
    let tex_00 = "textures/4096_earth_clouds.jpg"
    tx0 <- loadTex tex_00
    texture Texture2D        $= Enabled
    textureBinding Texture2D $= Just tx0

    return $ Descriptor vao (fromIntegral numIndices)
    
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Repeated, ClampToEdge)
    return t
