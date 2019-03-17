{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Rendering
  ( openWindow
  , closeWindow
  , draw
  --, initIntroResources
  , initGameResources
  , initResources
  , Descriptor(..)
  , Drawable(..)
  ) where

import Control.Monad
import Data.Text                              (Text)
import Foreign.C
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
import Foreign.Storable                       (sizeOf)
import Graphics.GLUtil (readTexture, texture2DWrap)
import Graphics.Rendering.OpenGL as GL hiding (position, Size)
import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot)
import SDL.Raw.Video
import SDL.Raw.Enum

import LoadShaders
import Game
import Geometry
import Drawables
import Shape2D

resX = 800 :: Int
resY = 600 :: Int

toTexCoord2 :: (a, a, a) -> TexCoord2 a
toTexCoord2 (k, l, m) = TexCoord2 k l

toTexCoord2s :: [(a, a, a)] -> [TexCoord2 a]
toTexCoord2s = map toTexCoord2 

instance Drawables Geo where
  toDrawable :: Geo -> IO Drawable
  toDrawable geo =
    do
      let ps  = map toVertex4   $ position geo
          uvs = map toTexCoord2 $ uv geo
          ids = indices geo
      return $ Drawable ps uvs ids


openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"  
     
    window <- SDL.createWindow
            "e1337"
            SDL.defaultWindow {SDL.windowInitialSize = V2 sizex sizey,
                               SDL.windowOpenGL = Just SDL.defaultOpenGL}
    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window = do
    SDL.destroyWindow window
    SDL.quit

-- < OpenGL > -------------------------------------------------------------
data Descriptor =
     Descriptor VertexArrayObject ArrayIndex NumArrayIndices

draw :: SDL.Window -> Game -> IO ()
draw window game =
  do
    (Descriptor vao firstIndex numVertices) <- initResources game

    GL.clearColor $= Color4 1 0 0 1
    GL.clear [ColorBuffer]
    bindVertexArrayObject $= Just vao
    drawArrays Triangles firstIndex numVertices

    -- multisampling
    glSetAttribute SDL_GL_MULTISAMPLEBUFFERS 1
    glSetAttribute SDL_GL_MULTISAMPLESAMPLES 16

    SDL.glSwapWindow window

realToFracT :: (Double, Double) -> (GLfloat, GLfloat)
realToFracT = (\ (x,y) -> (realToFrac x, realToFrac y))

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Repeated, ClampToEdge)
    return t

initGameResources :: Game -> IO Descriptor
initGameResources game =  
  do
    drawable <- toDrawable $ (geometry . object) game
    
    -- | VAO
    vao <- genObjectName
    bindVertexArrayObject $= Just vao

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let vs          = verts drawable
        numVertices = length vs
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let indices    = ids drawable
        numIndices = length indices
    withArray indices $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * length indices)
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 8 * floatSize

    -- | Positions
    let vPosition  = AttribLocation 0
        posOffset  = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | UV
    let uvCoords   = AttribLocation 1
        uvOffset   = 6 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled

    -- | Shaders
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource "shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "shaders/shader.frag")]
    currentProgram $= Just program

    -- | Set Uniforms
    -- location0         <- get (uniformLocation program "fPPos")
    -- uniform location0 $= (realToFrac ppos :: GLfloat)

    -- location1         <- get (uniformLocation program "vBPos")
    -- uniform location1 $= (Vector2 (realToFrac $ fst bpos)
    --                               (realToFrac $ snd bpos) :: Vector2 GLfloat)

    location2         <- get (uniformLocation program "u_resolution")
    let u_res         = Vector2 (toEnum resX) (toEnum resY) :: Vector2 GLfloat
    uniform location2 $= u_res

    currentTime       <- SDL.time
    location3         <- get (uniformLocation program "u_time")
    uniform location3 $= (currentTime :: GLfloat)
    
    -- | Set Transform Matrix
    let tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ] :: [GLfloat]
          
    transform         <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location4         <- get (uniformLocation program "transform")
    uniform location4 $= transform
    
    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing

    --return $ Descriptor vao (fromIntegral numIndices)
    let firstIndex = 0
    return $ Descriptor vao firstIndex (fromIntegral numVertices)

-- debug, for references purposes
initResources :: Game -> IO Descriptor
initResources game = do
  
    vao <- genObjectName
    bindVertexArrayObject $= Just vao

    --
    -- Declaring VBO: vertices
    --
    drawable <- toDrawable $ (geometry . object) game
    let vs          = verts drawable
        numVertices = length vs

    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 4 GL.Double 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    --
    -- Declaring VBO: UVs
    --
    let uv  = uvs drawable

    textureBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just textureBuffer
    withArray uv $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head uv))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let uvCoords = AttribLocation 1
    vertexAttribPointer uvCoords $=
        (ToFloat, VertexArrayDescriptor 2 GL.Double 0 (bufferOffset firstIndex))
    vertexAttribArray uvCoords   $= Enabled

    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource "shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "shaders/shader.frag")]
    currentProgram $= Just program

    --
    -- Set Uniforms
    --
    location <- get (uniformLocation program "fTime")
    uniform location $= (realToFrac (scalar . object $ game) :: GLfloat)

    return $ Descriptor vao firstIndex (fromIntegral numVertices)    
    

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
