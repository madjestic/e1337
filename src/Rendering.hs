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
import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot)
import Graphics.GLUtil                        (readTexture, texture2DWrap)

import LoadShaders
import Game
import Object
import Camera    as C
import Controllable
import Descriptor
import Material

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

-- render' :: Backend -> SDL.Window -> Game -> IO ()
-- render' Rendering.OpenGL window game =
--   do
--     let uniforms = undefined
--           :: [ ( Material
--                , Vector2  GLfloat
--                , Vector2  GLfloat
--                , Float
--                , GLmatrix GLfloat
--                , GLmatrix GLfloat
--                , GLmatrix GLfloat ) ]
--         descriptors = undefined
--           :: [ Descriptor ]

--         pack = undefined --iniforms ++ descriptor
--           :: [ ( Material
--                , Vector2  GLfloat
--                , Vector2  GLfloat
--                , Float
--                , GLmatrix GLfloat
--                , GLmatrix GLfloat
--                , GLmatrix GLfloat
--                , Descriptor ) ]

--     mapM_ (\(_mat _mouse _res _time _proj _cam _trans _ds) ->
--              draw window _ds
--              initUniforms' _mat _mouse _res _time _proj _cam _trans

--         -- initUniforms per Descriptor
--         -- render Descriptor
             
--     return ()

-- foo
--   :: Material
--   -> Vector2  GLfloat -- u_mouse
--   -> Vector2  GLfloat -- u_resolution
--   -> Float            -- u_time
--   -> GLmatrix GLfloat -- projection matrix (perspective distortion)
--   -> GLmatrix GLfloat -- camera     matrix
--   -> GLmatrix GLfloat -- transform  matrix
--   -> Descriptor
--   -> IO ()
-- foo _mat _mouse _res _time _proj _cam _trans _ds =
--   initUniforms' _mat _mouse _res _time _proj _cam _trans
--   -- mapM_ (draw window) (concat ds)
--   return ()

data Uniforms
  =  Uniforms
     {
       u_mats  :: [Material]
     , u_mouse :: Vector2  GLfloat
     , u_time  :: Float
     , u_proj  :: GLmatrix GLfloat
     , u_cam   :: GLmatrix GLfloat
     , u_trans :: GLmatrix GLfloat
     } deriving Show

data Drawable
  =  Drawable
     { uniforms   :: Uniforms
     , descriptor :: Descriptor
     } deriving Show

unpackGame :: Game -> [Drawable]
unpackGame game = [(Drawable (Uniforms u_mats' u_mouse' u_time' u_proj' u_cam' u_trans') ds)]
  where
    ds       = undefined :: Descriptor
    nObjs    = undefined -- number of objects
    u_mats'  = undefined
    u_mouse' = undefined 
    u_time'  = undefined 
    u_proj'  = undefined 
    u_cam'   = undefined 
    u_trans' = undefined 
      

render :: Backend -> SDL.Window -> Game -> IO ()
render Rendering.OpenGL window game =
  do
    GL.clearColor $= Color4 0.5 0.5 1.0 1.0
    GL.clear [ColorBuffer, DepthBuffer]

    let uniforms = undefined :: [Uniforms]
    let ds = concat $ toListOf (objects . traverse . descriptors ) game :: [Descriptor]
    --let ds = 
    mapM_ (draw window) ds

    SDL.glSwapWindow window
render Vulkan _ _ = undefined

draw :: SDL.Window -> Descriptor -> IO ()
draw window (Descriptor vao numIndices) =
  do
    bindVertexArrayObject $= Just vao
    drawElements Triangles numIndices GL.UnsignedInt nullPtr
    
    GL.pointSize $= 10

    cullFace  $= Just Back
    depthFunc $= Just Less

-- TODO : initUniforms per Object
-- initUniforms' :: Object -> IO ()
-- initUniforms' obj =
--   do

--     -- | Shaders
--     -- _ <- DT.trace ("vertShader: " ++ show (_vertShader $ _materials obj )) $ return ()
--     -- _ <- DT.trace ("vertShader: " ++ show (_fragShader $ _materials obj )) $ return ()

--     return ()
    
--     -- program <- loadShaders [
--     --   ShaderInfo VertexShader
--     --     (FileSource (_vertShader $ (_materials $ (_objects $ game)!!0)!!0)), -- inito for first _objects: TODO: replace with fmap or whatever.
--     --   ShaderInfo FragmentShader
--     --     (FileSource (_fragShader $ (_materials $ (_objects $ game)!!0)!!0))
--     --   ]
--     -- currentProgram $= Just program

-- initUniforms'' :: Descriptor -> Material -> IO ()
-- initUniforms'' ds mat = undefined

initUniforms'
  :: Material
  -> Vector2  GLfloat -- u_mouse
  -> Vector2  GLfloat -- u_resolution
  -> Float            -- u_time
  -> GLmatrix GLfloat -- projection matrix (perspective distortion)
  -> GLmatrix GLfloat -- camera     matrix
  -> GLmatrix GLfloat -- transform  matrix
  -> IO ()
initUniforms' _mat _mouse _res _time _proj _cam _trans = 
  do
    -- | Shaders
    -- _ <- DT.trace ("vertShader: " ++ show (_vertShader $ (_materials $ (_objects $ game)!!0)!!0)) $ return ()
    -- _ <- DT.trace ("vertShader: " ++ show (_fragShader $ (_materials $ (_objects $ game)!!0)!!0)) $ return ()
    
    program <- loadShaders
      [ ShaderInfo VertexShader   (FileSource (_vertShader _mat ))
      , ShaderInfo FragmentShader (FileSource (_fragShader _mat )) ]
    currentProgram $= Just program

    -- | Set Uniforms
    location0         <- get (uniformLocation program "u_mouse")
    uniform location0 $= _mouse
    
    location1         <- get (uniformLocation program "u_resolution")
    uniform location1 $= _res

    location2         <- get (uniformLocation program "u_time")
    uniform location2 $= (_time :: GLfloat)
    
    location3         <- get (uniformLocation program "persp")
    uniform location3 $= _proj
    
    location4         <- get (uniformLocation program "_camera")
    uniform location4 $= _cam

    location5         <- get (uniformLocation program "transform")
    uniform location5 $= _trans
    
    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing

    return ()


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
