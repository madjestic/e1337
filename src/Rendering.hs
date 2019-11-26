{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Rendering
  ( openWindow
  , closeWindow
  , draw
  , initVAO
  , initUniforms
  , render
  , Backend (..)
  ) where

import Control.Monad
import Data.Text                              (Text)
import Foreign.C
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr)
import Foreign.Storable                       (sizeOf)
import Graphics.Rendering.OpenGL as GL hiding (color, normal, Size)
import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot, project)
import Graphics.GLUtil                        (readTexture, texture2DWrap)

import LoadShaders
import Game
import Object
import Camera    as C
import Controllable
import Descriptor
import Material
import Mouse
--- debug stuff
import Project (Project, path, models)
import Geometry hiding (materials)
---

import Data.Foldable     as DF (toList)
import Linear.Projection as LP (perspective)

import Unsafe.Coerce

import Control.Lens       hiding (transform, indexed)
import Debug.Trace as DT

data Backend
  = OpenGL | Vulkan

openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) =
  do
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
              title
              SDL.defaultWindow { SDL.windowInitialSize = V2 sizex sizey
                                , SDL.windowOpenGL      = Just config
                                }

    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window =
  do
    SDL.destroyWindow window
    SDL.quit

-- -- < OpenGL > -------------------------------------------------------------

data Uniforms
  =  Uniforms
     {
       u_mats  :: Material
     , u_mouse :: (Double, Double)
     , u_time  :: Float
     , u_res   :: (CInt, CInt)
     --, u_proj  :: M44 Double --GLmatrix GLfloat
     , u_cam   :: M44 Double 
     , u_trans :: M44 Double 
     } deriving Show

data Drawable
  =  Drawable
     { uniforms   :: Uniforms
     , descriptor :: Descriptor
     } deriving Show

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = map

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

toDrawables :: Game -> Float -> [Drawable]
toDrawables game time = drs --(DT.trace ("drs: " ++ show (length drs)) $ drs)
  where
    ds       = concat $ toListOf (objects . traverse . descriptors) game :: [Descriptor]
    n        = length ds :: Int
    u_mats   = concat $ toListOf (objects . traverse . materials) game :: [Material]
    u_mouse  = replicate n $ unsafeCoerce $ view (camera . controller . device . mouse . pos) game :: [(Double, Double)]
    u_time   = replicate n $ time      :: [Float]
    resX     = fromEnum $ view (options . resx) game :: Int
    resY     = fromEnum $ view (options . resy) game :: Int
    u_res    = replicate n $ ((toEnum resX), (toEnum resY)) :: [(CInt, CInt)]
--    u_proj   = undefined -- :: [GLmatrix GLfloat]
    u_cam    = replicate n $ view (camera . controller . Controllable.transform) game :: [M44 Double]
    u_trans  = concat $ replicate n $ toListOf (objects . traverse . Object.transform) game :: [M44 Double]  -- :: [GLmatrix GLfloat]
    drs      =
      (\  u_mats' u_mouse' u_time' u_res' u_cam' u_trans' ds'
        -> (Drawable (Uniforms u_mats' u_mouse' u_time' u_res' u_cam' u_trans') ds')) 
      <$.> u_mats <*.> u_mouse <*.> u_time <*.> u_res <*.> u_cam <*.> u_trans <*.> ds

render :: Backend -> SDL.Window -> Game -> Project -> IO ()
render Rendering.OpenGL window game proj =
  do
    GL.clearColor $= Color4 0.5 0.5 1.0 1.0
    GL.clear [ColorBuffer, DepthBuffer]

    (VGeo idxs st vaos matPaths) <- readVGeo $ path ((models proj)!!0)
    mats                         <- mapM readMaterial matPaths
    let args = (\idx' st' vao' mat' ->  (idx', st', vao', mat')) <$.> idxs <*.> st <*.> vaos <*.> mats
    
    ticks             <- SDL.ticks
    let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float
        drs = toDrawables game currentTime

    mapM_ (draw window) drs

    SDL.glSwapWindow window
    
render Vulkan _ _ _ = undefined

draw :: SDL.Window -> Drawable -> IO ()
draw window (Drawable
              unis
              (Descriptor vao' numIndices')) =
  do
    --_ <- DT.trace ("ds: " ++ show (ds)) $ return ()
    
    initUniforms unis
    
    bindVertexArrayObject $= Just vao'
    drawElements Triangles numIndices' GL.UnsignedInt nullPtr
    
    GL.pointSize $= 10

    cullFace  $= Just Back
    depthFunc $= Just Less

initUniforms :: Uniforms -> IO ()
initUniforms (Uniforms u_mats' u_mouse' u_time' u_res' u_cam' u_trans') = 
  do
    -- | Shaders
    -- _ <- DT.trace ("vertShader: " ++ show (_vertShader $ (_materials $ (_objects $ game)!!0)!!0)) $ return ()
    -- _ <- DT.trace ("vertShader: " ++ show (_fragShader $ (_materials $ (_objects $ game)!!0)!!0)) $ return ()
    
    program <- loadShaders
      [ ShaderInfo VertexShader   (FileSource (_vertShader u_mats' ))
      , ShaderInfo FragmentShader (FileSource (_fragShader u_mats' )) ]
    currentProgram $= Just program

    -- | Set Uniforms
    let u_mouse       = Vector2 (realToFrac $ fst u_mouse') (realToFrac $ snd u_mouse') :: Vector2 GLfloat
    location0         <- get (uniformLocation program "u_mouse'")
    uniform location0 $= u_mouse

    let resX = fromIntegral $ fromEnum $ fst u_res' :: Float
        resY = fromIntegral $ fromEnum $ snd u_res' :: Float
        
    let u_res         = Vector2 resX resY :: Vector2 GLfloat
           
    location1         <- get (uniformLocation program "u_resolution")
    uniform location1 $= u_res

    location2         <- get (uniformLocation program "u_time'")
    uniform location2 $= (u_time' :: GLfloat)


    let proj =          
          fmap realToFrac . concat $ fmap DF.toList . DF.toList -- convert to GLfloat
          --               FOV    Aspect    Near   Far
          $ LP.perspective (pi/2.0) (resX/resY) (0.01) 1.5 :: [GLfloat]

    persp             <- GL.newMatrix RowMajor proj :: IO (GLmatrix GLfloat)
    location3         <- get (uniformLocation program "persp")
    uniform location3 $= persp

    let cam =
          fmap realToFrac . concat $ fmap DF.toList . DF.toList $ u_cam'
    camera            <- GL.newMatrix RowMajor cam :: IO (GLmatrix GLfloat)
    location4         <- get (uniformLocation program "camera")
    uniform location4 $= camera

    let mtx =
          fmap realToFrac . concat $ fmap DF.toList . DF.toList $ --u_trans'
          (identity::M44 Double) :: [GLfloat]
    transform         <- GL.newMatrix RowMajor mtx :: IO (GLmatrix GLfloat)
    location5         <- get (uniformLocation program "transform")
    uniform location5 $= transform --u_trans'
    
    -- | Unload buffers
    --bindVertexArrayObject         $= Nothing
    --bindBuffer ElementArrayBuffer $= Nothing

    return ()      

       -- | Indices -> Stride -> ListOfFloats -> Material -> Descriptor
initVAO :: ([Int], Int, [Float], Material) -> IO Descriptor
initVAO (idx', st, vs', matPath) =  
  do
    let
      idx = (fmap unsafeCoerce idx') :: [GLuint]
      vs  = (fmap unsafeCoerce vs')  :: [GLfloat]
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

    return $ Descriptor vao (fromIntegral numIndices)

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Repeated, ClampToEdge)
    return t
