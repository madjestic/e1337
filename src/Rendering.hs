{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Rendering
  ( openWindow
  , closeWindow
  , draw
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
import Data.List.Split
import Data.List.Index

import LoadShaders
import Game
import Geometry
import Drawables
import Shape2D

import Data.Set          as DS (fromList, toList)
import Data.Foldable     as DF (toList)
import Linear.Projection as LP (perspective, inversePerspective, frustum)

import Debug.Trace as DT

resX = 800 :: Int
resY = 600 :: Int

toTexCoord3 :: (a, a, a) -> TexCoord3 a
toTexCoord3 (k, l, m) = TexCoord3 k l m

instance Drawables Geo where
  toDrawable :: Geo -> IO Drawable
  toDrawable geo =
    do
      let ps  = map toVertex4   $ position geo
          uvs = map toTexCoord3 $ uv geo
          ids = map toGLuint $ indices geo
      return $ Drawable ps uvs ids

toGLuint :: Int -> GLuint
toGLuint x = fromIntegral x

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
     --Descriptor VertexArrayObject ArrayIndex NumArrayIndices
     Descriptor VertexArrayObject NumArrayIndices

draw :: SDL.Window -> Game -> IO ()
draw window game =
  do
    --(Descriptor vao firstIndex numVertices) <- initResources game
    (Descriptor vao numIndices) <- initResources game

    GL.clearColor $= Color4 1 0 0 1
    GL.clear [ColorBuffer, DepthBuffer]
    bindVertexArrayObject $= Just vao
    --drawArrays Triangles firstIndex numVertices
    drawElements Triangles numIndices GL.UnsignedInt nullPtr
    GL.pointSize $= 10

    cullFace  $= Just Back
    depthFunc $= Just Less

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

initVAO :: [Vertex4 Double] -> [TexCoord3 Double] -> [GLuint] -> IO [GLfloat]
initVAO ps ts idx =
  return $ concat $
    fmap (\i -> (\(Vertex4 x y z w) -> fmap realToFrac [x, y, z, w]) (ps!!(fromIntegral (idx!!i))) ++
                (\(TexCoord3 u v w) -> fmap realToFrac [u, v, w])    (ts!!(fromIntegral       i))) iter
      where
        iter = [0..(length idx)-1]

indexedVAO :: [Vertex4 Double] -> [TexCoord3 Double] -> [GLuint] -> Int -> IO ([GLfloat],[GLuint])
indexedVAO ps ts ids st =
  do
    vs <- initVAO ps ts ids
    let iListSet = indexedListSet vs st
    let iList    = indexedList (indexed $ chunksOf st vs) iListSet
    let idx = fmap (\(i,_) -> (fromIntegral i)) iList
    let vx  = concat $ fmap (\x -> snd x) iListSet
    return (vx, idx)

         -- :: initVAO   -> Stride -> indexed VAO
indexedListSet :: [GLfloat] -> Int -> [(Int,[GLfloat])]
indexedListSet vao n =
  fmap (\x -> x) $ indexed $ DS.toList $ DS.fromList $ chunksOf n $ vao

indexedList :: [(Int,[GLfloat])] -> [(Int,[GLfloat])] -> [(Int,[GLfloat])]
indexedList loa ias = foldr (\x y -> matchIndex y x) loa ias

matchIndex :: [(Int,[GLfloat])] -> (Int,[GLfloat]) -> [(Int,[GLfloat])]
matchIndex loa indices@(i, iVal) = fmap (\la@(j, jVal) -> case () of
                                                _ | iVal == jVal -> indices
                                                  | otherwise    -> la) loa

fromIndex :: [(Int,[GLfloat])] -> [Int] -> IO [GLfloat]
fromIndex ias is = return $ concat $ fmap (\i -> snd (ias!!i)) is

initResources :: Game -> IO Descriptor
initResources game =  
  do
    drw <- toDrawable $ (geometry . object) game
  
    let stride = 7 -- TODO : stride <- attr sizes
    (vs, idx) <- indexedVAO (verts drw) (uvs drw) (ids drw) stride

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
        stride     =  7 * floatSize

    -- | Positions
    let vPosition  = AttribLocation 0
        posOffset  = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 4 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | UV
    let uvCoords   = AttribLocation 1
        uvOffset   = 4 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset uvOffset))
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
    
    -- -- | Set Transform Matrix
    -- let tr' =
    --       [ 1, 0, 0, 0
    --       , 0, 1, 0, 0
    --       , 0, 0, 1, 0
    --       , 0, 0, 0, 0.5 ] :: [GLfloat]

    -- TODO : replace NDC with ortho from http://hackage.haskell.org/package/linear-1.20.8/docs/Linear-Projection.html
    -- let ndc = fmap realToFrac . concat
    --         $ fmap DF.toList . DF.toList
    --         $ (identity::M44 Double) :: [GLfloat]

    let tr =
            fmap realToFrac . concat
          $ fmap DF.toList . DF.toList
          $ (transform . object) game :: [GLfloat]

    let persp = fmap realToFrac . concat $ fmap DF.toList . DF.toList $ LP.perspective (pi/2) (800/600) (0.35) 1.5 :: [GLfloat]
          
    transform         <- GL.newMatrix ColumnMajor persp :: IO (GLmatrix GLfloat)
    location4         <- get (uniformLocation program "transform")
    uniform location4 $= transform
    
    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing

    return $ Descriptor vao (fromIntegral numIndices)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
