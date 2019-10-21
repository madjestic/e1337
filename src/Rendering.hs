{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Rendering
  ( openWindow
  , closeWindow
  , draw
  , initVAO'
  , initUniforms
  , toDrawable
  , Descriptor(..)
  , Drawable(..)
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
    GL.clearColor $= Color4 0.5 0.5 1.0 1.0
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
        (FileSource (vertShader $ (material $ (object $ game)!!0)!!0)), -- inito for first object: TODO: replace with fmap or whatever.
      ShaderInfo FragmentShader
        (FileSource (fragShader $ (material $ (object $ game)!!0)!!0))
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
           where resX = fromEnum $ (resx . options $ game)
                 resY = fromEnum $ (resy . options $ game)
    uniform location1 $= u_res

    ticks             <- SDL.ticks
    let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float
    location2         <- get (uniformLocation program "u_time")
    uniform location2 $= (currentTime :: GLfloat)
    
    let proj =          
          fmap realToFrac . concat $ fmap DF.toList . DF.toList -- convert to GLfloat
          --               FOV    Aspect    Near   Far
          $ LP.perspective (pi/2) (resX/resY) (0.01) 1.5 :: [GLfloat]
                     where resX = toEnum $ fromEnum $ (resx . options $ game)
                           resY = toEnum $ fromEnum $ (resy . options $ game)

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

instance ToDrawable FilePath where
  toDrawable modelPath = do
    geo <- (\x -> case (reverse . take 4 . reverse $ x) of
                    "pgeo" -> readPGeo   x
                    "vgeo" -> readVBOGeo x ) modelPath

    _ <- DT.trace ("toDrawable: geo:" ++ show geo) $ return ()
    drw <- (\x -> case x of
             Geo indices alpha color normal uv positions materials
               -> fromGeo (Geo indices alpha color normal uv positions materials)
             GLGeo vs idx
               -> return $ Drawable vs is'
               where
                 is'  = [(map fromIntegral (idx!!0))] :: [[GLuint]]) geo
    _ <- DT.trace ("toDrawable: drw:" ++ show drw) $ return ()
    return drw

initVAO :: Game -> IO Descriptor
initVAO game =  
  do
    print game
    _ <- DT.trace ("trace 1_0: " ++ show (geoPath $ (object game)!!0)) $ return ()
    (Drawable vs idx) <- toDrawable $ geoPath $ (object game)!!0 -- take first object, TODO: replace with fmap or whatever.
    -- | VAO
    vao <- genObjectName
    bindVertexArrayObject $= Just vao 
    _ <- DT.trace ("trace 1_1: " ++ show vs ++ show " " ++ show idx) $ return ()

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    _ <- DT.trace ("trace 1_2: " ++ show vs) $ return ()
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral ((length vs) * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    _ <- DT.trace ("trace 1_3: " ++ show (idx!!0)) $ return ()
    let numIndices = length (idx!!0)
    withArray (idx!!0) $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * sizeOf (head (idx!!0)))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
        -- | Bind the pointer to the vertex attribute data
        let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            stride     =  14 * floatSize -- TODO : stride value should come from a single location
        
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
        vertexAttribPointer (AttribLocation 4) $= (ToFloat, VertexArrayDescriptor 4 Float stride ((plusPtr nullPtr . fromIntegral) (10 * floatSize)))
        vertexAttribArray   (AttribLocation 4) $= Enabled
        
        -- | Assign Textures
        activeTexture            $= TextureUnit 0
        texture Texture2D        $= Enabled
        tx0 <- loadTex "textures/4096_earth_clouds.jpg"
        textureBinding Texture2D $= Just tx0
        
    return $ Descriptor vao (fromIntegral numIndices)

initVAO' :: Object -> IO Descriptor
initVAO' obj =  
  do
    print obj
    _ <- DT.trace ("trace 1_0: " ++ show (geoPath obj)) $ return ()
    (Drawable vs idx) <- toDrawable $ geoPath obj -- take first object, TODO: replace with fmap or whatever.
    -- | VAO
    vao <- genObjectName
    bindVertexArrayObject $= Just vao 
    _ <- DT.trace ("trace 1_1: " ++ show vs ++ show " " ++ show idx) $ return ()

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    _ <- DT.trace ("trace 1_2: " ++ show vs) $ return ()
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral ((length vs) * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    _ <- DT.trace ("trace 1_3: " ++ show (idx!!0)) $ return ()
    let numIndices = length (idx!!0)
    withArray (idx!!0) $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * sizeOf (head (idx!!0)))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
        -- | Bind the pointer to the vertex attribute data
        let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            stride     =  14 * floatSize -- TODO : stride value should come from a single location
        
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
        vertexAttribPointer (AttribLocation 4) $= (ToFloat, VertexArrayDescriptor 4 Float stride ((plusPtr nullPtr . fromIntegral) (10 * floatSize)))
        vertexAttribArray   (AttribLocation 4) $= Enabled
        
        -- | Assign Textures
        activeTexture            $= TextureUnit 0
        texture Texture2D        $= Enabled
        tx0 <- loadTex "textures/4096_earth_clouds.jpg"
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
