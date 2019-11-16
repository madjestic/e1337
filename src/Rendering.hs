{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Rendering
  ( openWindow
  , closeWindow
  , draw
  , initVAO
  , initVAO'
  -- , initResources
  , initUniforms
--  , Descriptor(..)
  ) where

import Control.Monad
import Data.Text                              (Text)
import Foreign.C
import Foreign.Marshal.Array                  (withArray, newArray)
import Foreign.Ptr                            (plusPtr, nullPtr, alignPtr, Ptr)
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
import Descriptor
import Shape2D
import Material
import Utils

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
                              , glProfile = Compatibility Normal 2 1
                              }

    depthFunc $= Just Less

    window <- SDL.createWindow
              "e1337"
              SDL.defaultWindow { SDL.windowInitialSize = V2 sizex sizey
                                , SDL.windowOpenGL      = Just config
                                }

    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window = do
    SDL.destroyWindow window
    SDL.quit

-- -- < OpenGL > -------------------------------------------------------------

draw :: SDL.Window -> Descriptor -> IO ()
draw window (Descriptor vao numIndices) =
  do
    GL.clearColor $= Color4 0.5 0.5 1.0 1.0
    GL.clear [ColorBuffer, DepthBuffer]
    
    bindVertexArrayObject $= Just vao
    drawElements Triangles numIndices GL.UnsignedInt nullPtr
    
    GL.pointSize $= 10

    cullFace  $= Just Back
    depthFunc $= Just Less

    --SDL.glSwapWindow window

initUniforms :: Game -> IO ()
initUniforms game =  
  do
    -- | Shaders
    -- _ <- DT.trace ("vertShader: " ++ show (_vertShader $ (_materials $ (_objects $ game)!!0)!!0)) $ return ()
    -- _ <- DT.trace ("vertShader: " ++ show (_fragShader $ (_materials $ (_objects $ game)!!0)!!0)) $ return ()
    
    program <- loadShaders [
      ShaderInfo VertexShader
        (FileSource (_vertShader $ (_materials $ (_objects $ game)!!0)!!0)), -- inito for first _objects: TODO: replace with fmap or whatever.
      ShaderInfo FragmentShader
        (FileSource (_fragShader $ (_materials $ (_objects $ game)!!0)!!0))
      ]
    currentProgram $= Just program

    -- | Set Uniforms
    location0         <- get (uniformLocation program "u_mouse")
    let u_mouse       = Vector2 (realToFrac $ fst mpos) (realToFrac $ snd mpos) :: Vector2 GLfloat
           where mpos = 
                   (pos . mouse . devices . C.controller . _camera $ game)
    uniform location0 $= u_mouse
    
    location1         <- get (uniformLocation program "u_resolution")
    let u_res         = Vector2 (toEnum resX) (toEnum resY) :: Vector2 GLfloat
           where resX = fromEnum $ (_resx . _options $ game)
                 resY = fromEnum $ (_resy . _options $ game)
    uniform location1 $= u_res

    ticks             <- SDL.ticks
    let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float
    location2         <- get (uniformLocation program "u_time")
    uniform location2 $= (currentTime :: GLfloat)
    
    let proj =          
          fmap realToFrac . concat $ fmap DF.toList . DF.toList -- convert to GLfloat
          --               FOV    Aspect    Near   Far
          $ LP.perspective (pi/2) (resX/resY) (0.01) 1.5 :: [GLfloat]
                     where resX = toEnum $ fromEnum $ (_resx . _options $ game)
                           resY = toEnum $ fromEnum $ (_resy . _options $ game)

    persp             <- GL.newMatrix RowMajor proj :: IO (GLmatrix GLfloat)
    location3         <- get (uniformLocation program "persp")
    uniform location3 $= persp
    
    let cam =
          fmap realToFrac . concat $ fmap DF.toList . DF.toList $
          transform . C.controller . _camera $ game --(identity::M44 Double) :: [GLfloat]
    _camera            <- GL.newMatrix RowMajor cam :: IO (GLmatrix GLfloat)
    location4         <- get (uniformLocation program "_camera")
    uniform location4 $= _camera

    let mtx =
          fmap realToFrac . concat $ fmap DF.toList . DF.toList $
          (identity::M44 Double) :: [GLfloat]
    transform         <- GL.newMatrix RowMajor mtx :: IO (GLmatrix GLfloat)
    location5         <- get (uniformLocation program "transform")
    uniform location5 $= transform
    
    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing

    return () -- $ Descriptor vao (fromIntegral numIndices)    

       -- | Indices -> Stride -> ListOfFloats -> Material -> Descriptor
initVAO :: ([Int], Int, [Float], Material) -> IO Descriptor
initVAO (idx, st, vs, matPath) =  
  do
    -- _ <- DT.trace ("idx: "     ++ show idx)    $ return ()
    -- _ <- DT.trace ("stride: "  ++ show st) $ return ()
    -- _ <- DT.trace ("vs: "      ++ show vs)     $ return ()
    -- _ <- DT.trace ("matPath: " ++ show matPath)     $ return ()
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
    let numIndices = length (idx)
    --_ <- DT.trace ("idx: " ++ show idx) $ return ()
    withArray (idx) $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * sizeOf (head (idx)))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
        -- | Bind the pointer to the vertex attribute data
        let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            stride     = (fromIntegral st) * floatSize -- TODO : stride value should come from a single location
        
        -- | Alpha
        vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 1 Float stride ((plusPtr nullPtr . fromIntegral) (0 * floatSize)))
        vertexAttribArray   (AttribLocation 0) $= Enabled
        -- | Colors
        vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (1 * floatSize)))
        vertexAttribArray   (AttribLocation 1) $= Enabled
        -- | Normals
        vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (4 * floatSize)))
        vertexAttribArray   (AttribLocation 2) $= Enabled
        -- | UV
        vertexAttribPointer (AttribLocation 3) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (7 * floatSize)))
        vertexAttribArray   (AttribLocation 3) $= Enabled
        -- | Positions
        vertexAttribPointer (AttribLocation 4) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (10 * floatSize)))
        vertexAttribArray   (AttribLocation 4) $= Enabled
        
        -- | Assign Textures
        activeTexture            $= TextureUnit 0
        texture Texture2D        $= Enabled
        tx0 <- loadTex "textures/4096_earth_clouds.jpg"
        textureBinding Texture2D $= Just tx0

    -- _ <- DT.trace ("vao: " ++ show vao) $ return ()            -- trace vao, numIndices
    -- _ <- DT.trace ("numIndices: " ++ show numIndices) $ return ()
    return $ Descriptor vao (fromIntegral numIndices)

initVAO' :: ([Int], Int, [Float], Material) -> IO Descriptor
initVAO' (idx, st, vs, matPath) =  
  do
    let vs = [0.999,0.510895,0.79206765,0.7084489,0.0,0.0,-1.0,1.0,1.0,0.5,    0.5, 0.5,0.0,
              0.999,0.6416006,0.8147795,8.0804706e-2,0.0,0.0,-1.0,0.0,0.0,0.5,-0.5,-0.5,0.0,
              0.999,0.8004638,0.7638743,0.31464565,0.0,0.0,-1.0,1.0,0.0,0.5,   0.5,-0.5,0.0] :: [GLfloat]
        idx = [1,2,0] :: [GLuint]
    -- _ <- DT.trace ("idx: " ++ show idx) $ return ()
    -- _ <- DT.trace ("vs:  " ++ show vs) $ return ()
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
    --_ <- DT.trace ("idx: " ++ show idx) $ return ()
    withArray idx $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * sizeOf (head (idx)))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
        -- | Bind the pointer to the vertex attribute data
        let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            stride     =  13 * floatSize -- TODO : stride value should come from a single location
        
        -- | Alpha
        vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 1 Float stride ((plusPtr nullPtr . fromIntegral) (0 * floatSize)))
        vertexAttribArray   (AttribLocation 0) $= Enabled
        -- | Colors
        vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (1 * floatSize)))
        vertexAttribArray   (AttribLocation 1) $= Enabled
        -- | Normals
        vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (4 * floatSize)))
        vertexAttribArray   (AttribLocation 2) $= Enabled
        -- | UV
        vertexAttribPointer (AttribLocation 3) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (7 * floatSize)))
        vertexAttribArray   (AttribLocation 3) $= Enabled
        -- | Positions
        vertexAttribPointer (AttribLocation 4) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (10 * floatSize)))
        vertexAttribArray   (AttribLocation 4) $= Enabled
        
        -- | Assign Textures
        activeTexture            $= TextureUnit 0
        texture Texture2D        $= Enabled
        tx0 <- loadTex "textures/4096_earth_clouds.jpg"
        textureBinding Texture2D $= Just tx0

    -- _ <- DT.trace ("vao: " ++ show vao) $ return ()            -- trace vao, numIndices
    -- _ <- DT.trace ("numIndices: " ++ show numIndices) $ return ()
    return $ Descriptor vao (fromIntegral numIndices)
    
-- bufferOffset :: Integral a => a -> Ptr b
-- bufferOffset = plusPtr nullPtr . fromIntegral

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Repeated, ClampToEdge)
    return t
