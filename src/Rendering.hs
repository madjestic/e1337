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
--  , toDrawables
--  , Descriptor(..)
--  , Drawable(..)
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

-- -- draw' :: SDL.Window -> Descriptor -> Material -> M44 Double -> IO ()
-- -- draw' window (Descriptor vao numIndices) mat tr = undefined

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

-- draw' :: Descriptor -> IO ()
-- draw' (Descriptor vao numIndices) =
--   do
--     GL.clearColor $= Color4 0.5 0.5 1.0 1.0
--     GL.clear [ColorBuffer, DepthBuffer]
--     bindVertexArrayObject $= Just vao
--     drawElements Triangles numIndices GL.UnsignedInt nullPtr
--     GL.pointSize $= 10

--     cullFace  $= Just Back
--     depthFunc $= Just Less
    
    
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


-- -- instance ToDrawable Object where
-- --   toDrawables :: Object -> [Drawable]
-- --   toDrawables obj = undefined --do

-- -- instance ToDrawable Object where
-- --   toDrawables :: Object -> IO [Drawable]
-- --   toDrawables obj = undefined --do
--     -- -- :: Geo
--     -- geo <- (\x -> case (reverse . take 4 . reverse $ x) of
--     --                 "pgeo" -> readPGeo   x
--     --                 "vgeo" -> readVBOGeo x ) modelPath
--     -- -- :: Drawable
--     -- drw <- (\x -> case x of
--     --          PGeo indices alpha color normal uv positions materials
--     --            -> fromPGeo (PGeo indices alpha color normal uv positions materials)
--     --          VGeo vs idx
--     --            -> return $ Drawable vs is'
--     --            where
--     --              is'  = (map fromIntegral (idx)) :: [GLuint]
--     --        ) geo
--     -- return drw

--   -- toDrawables :: FilePath -> IO [Drawable]
--   -- toDrawables modelPath = do
--   --   geo <- (\x -> case (reverse . take 4 . reverse $ x) of
--   --                   "pgeo" -> readPGeo   x
--   --                   "vgeo" -> readVBOGeo x ) modelPath

--   --   drw <- (\x -> case x of
--   --            PGeo indices alpha color normal uv positions materials
--   --              -> fromPGeo (PGeo indices alpha color normal uv positions materials)
--   --            VGeo vs idx
--   --              -> return $ Drawable vs is'
--   --              where
--   --                is'  = (map fromIntegral (idx)) :: [GLuint]
--   --          ) geo
--   --   return [drw]
  
    
-- initResources :: [Object] -> IO [Descriptor]
-- initResources objs =
--   do
--     let matIdxRng = fromObject (objs!!0) :: [[GLuint]]
--     descr <- mapM fromMaterial matIdxRng
--     return descr

-- -- | Object -> [MaterialIndices]
-- fromObject :: Object -> [[GLuint]]
-- fromObject obj = undefined
    

--         -- | Material -> Descriptor
-- fromMaterial :: [GLuint] -> IO Descriptor
-- fromMaterial _ = undefined

-- initVAO :: Object -> IO Descriptor
-- initVAO obj =  
--   do
--     _ <- DT.trace ("obj: " ++ show obj) $ return ()
--     --(Drawable vs idx) <- (toDrawables obj) -- take first _objects, TODO: replace with fmap or whatever.
--     -- ds <- (toDrawables obj)
--     let
--       ds  = toDrawables obj
--       vs  = verts (ds!!0)
--       idx = ids   (ds!!0)
--     _ <- DT.trace ("idx: " ++ show idx) $ return ()
--     -- | VAO
--     vao <- genObjectName
--     bindVertexArrayObject $= Just vao 
--     -- | VBO
--     vertexBuffer <- genObjectName
--     bindBuffer ArrayBuffer $= Just vertexBuffer
--     withArray vs $ \ptr ->
--       do
--         let sizev = fromIntegral ((length vs) * sizeOf (head vs))
--         bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)
--     -- | EBO
--     elementBuffer <- genObjectName
--     bindBuffer ElementArrayBuffer $= Just elementBuffer
--     let numIndices = length (idx)
--     --_ <- DT.trace ("idx: " ++ show idx) $ return ()
--     withArray (idx) $ \ptr ->
--       do
--         let indicesSize = fromIntegral (numIndices * sizeOf (head (idx)))
--         bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
--         -- | Bind the pointer to the vertex attribute data
--         let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
--             stride     =  14 * floatSize -- TODO : stride value should come from a single location
        
--         -- | Alpha
--         vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 1 Float stride ((plusPtr nullPtr . fromIntegral) (0 * floatSize)))
--         vertexAttribArray   (AttribLocation 0) $= Enabled
--         -- | Colors
--         vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (1 * floatSize)))
--         vertexAttribArray   (AttribLocation 1) $= Enabled
--         -- | Normals
--         vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (4 * floatSize)))
--         vertexAttribArray   (AttribLocation 2) $= Enabled
--         -- | UV
--         vertexAttribPointer (AttribLocation 3) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (7 * floatSize)))
--         vertexAttribArray   (AttribLocation 3) $= Enabled
--         -- | Positions
--         vertexAttribPointer (AttribLocation 4) $= (ToFloat, VertexArrayDescriptor 4 Float stride ((plusPtr nullPtr . fromIntegral) (10 * floatSize)))
--         vertexAttribArray   (AttribLocation 4) $= Enabled
        
--         -- | Assign Textures
--         activeTexture            $= TextureUnit 0
--         texture Texture2D        $= Enabled
--         tx0 <- loadTex "textures/4096_earth_clouds.jpg"
--         textureBinding Texture2D $= Just tx0
        
--     return $ Descriptor vao (fromIntegral numIndices)

data Drawable
  =  Drawable
     { verts  :: [GLfloat]
     , ids    :: [GLuint] -- [[GLuint]]
     } deriving Show


toDrawable modelPath = do
  geo <- (\x -> case (reverse . take 4 . reverse $ x) of
                  "pgeo" -> readPGeo   x
                  "vgeo" -> readVGeo x ) modelPath

  drw <- (\x -> case x of
           PGeo indices alpha color normal uv positions materials
             -> fromGeo (PGeo indices alpha color normal uv positions materials)
           VGeo is st vs ms
             -> return $ Drawable (fmap unsafeCoerce $ vs!!0) (fmap fromIntegral $ is!!0)
             where
               is'  = [(map fromIntegral $ is!!0)] :: [[GLuint]]) geo
  return drw

newtype GeoPath = GeoPath String deriving Show

toVAO'
  :: [[GLuint]]
  -> [Float]
  -> [Vertex3 Double]
  -> [Vertex3 Double]
  -> [TexCoord3 Double]
  -> [Vertex4 Double]
  -> IO [GLfloat]
toVAO' idx as cds ns ts ps =
  do
    -- _ <- DT.trace ("toVAO: idx:" ++ show idx) $ return ()
    -- _ <- DT.trace ("toVAO: ps:" ++ show ps) $ return ()
    return $ concat $
      fmap (\i ->
              (\x -> [x])                                            (as !!(fromIntegral       i)) ++ -- 1
              (\(Vertex3   r g b)   -> fmap realToFrac [r,g,b])      (cds!!(fromIntegral       i)) ++ -- 3
              (\(Vertex3   x y z)   -> fmap realToFrac [x,y,z])      (ns !!(fromIntegral       i)) ++ -- 3
              (\(TexCoord3 u v w)   -> fmap realToFrac [u, v, w])    (ts !!(fromIntegral       i)) ++ -- 3
              (\(Vertex4   x y z w) -> fmap realToFrac [x, y, z, w]) (ps !!(fromIntegral (DT.trace ("((idx!!1)!!i) :" ++ show ((idx!!1)!!i)) $ ((idx!!1)!!i))))   -- 4 -> stride 14 -- this shit draws different tris
           ) iter
        where
          iter = [0..(length (idx))-1]


fromGeo :: Geo -> IO Drawable
fromGeo geo = do
  let stride = 14 -- TODO : stride <- attr sizes
  vs <- (toVAO' ids' as' cds' ns' uv' ps') :: IO [GLfloat]
  --(vs, idx) <- (toIdxVAO ids' as' cds' ns' uv' ps' stride) :: IO ([GLfloat],[GLuint])
  -- _ <- DT.trace ("geo: "   ++ show geo) $ return ()
  -- _ <- DT.trace ("fromGeo vs: "   ++ show vs) $ return ()
  -- _ <- DT.trace ("fromGeo ids': " ++ show ids') $ return ()
  -- _ <- DT.trace ("fromGeo uid: " ++ show uid) $ return ()
  -- _ <- DT.trace ("ps': " ++ show ps') $ return ()
  --return (Drawable vs idx)
  return (Drawable vs uid)
  --return (Drawable vs ids')
    where
      ids' = (fmap . fmap) fromIntegral $ indices geo                        -- index
      --uid  = [[0,1,2],[3,4,5]] --(map fromIntegral [0..((length (ids'!!0))-1)] :: [GLuint]) -- index per pt, as in uncompressed, needed for non-indexed geo to work
      uid  = (map fromIntegral [0..((length (ids'!!0))-1)] :: [GLuint]) -- index per pt, as in uncompressed, needed for non-indexed geo to work
      as'  = alpha geo
      cds' = map (\ (r, g, b) -> Vertex3   r g b) $ color  geo
      ns'  = map (\ (x, y, z) -> Vertex3   x y z) $ normal geo
      uv'  = map (\ (k, l, m) -> TexCoord3 k l m) $ uv     geo
      ps'  = map toVertex4 $ Geometry.position geo -- TODO : grouping should happen here, based on material index

initVAO' :: IO Descriptor
initVAO' =  
  do
    --_ <- DT.trace ("obj: " ++ show obj) $ return ()
    (Drawable vs idx) <- toDrawable $ "models/square.vgeo" -- take first objects, TODO: replace with fmap or whatever.
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


       -- | Indices -> Stride -> ListOfFloats -> Material -> Descriptor
initVAO :: ([Int], Int, [Float], Material) -> IO Descriptor
initVAO (idx, stride, vs, matPath) =  
  do
    -- _ <- DT.trace ("obj: " ++ show obj) $ return ()
    -- (Drawable vs idx) <- (toDrawables obj) -- take first _objects, TODO: replace with fmap or whatever.
    -- ds <- (toDrawables obj)
    let
      --ds  = toDrawables obj
      --vs  = verts (ds!!0)
      --idx = ids   (ds!!0)
    -- _ <- DT.trace ("idx: "    ++ show idx)    $ return ()
    -- _ <- DT.trace ("stride: " ++ show stride) $ return ()
    -- _ <- DT.trace ("vs: "     ++ show vs)     $ return ()
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
        vertexAttribPointer (AttribLocation 4) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (10 * floatSize)))
        vertexAttribArray   (AttribLocation 4) $= Enabled
        
        -- | Assign Textures
        activeTexture            $= TextureUnit 0
        texture Texture2D        $= Enabled
        tx0 <- loadTex "textures/4096_earth_clouds.jpg"
        textureBinding Texture2D $= Just tx0
        
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
