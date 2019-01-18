{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module Main where 

import Control.Concurrent
import Control.Monad
import Data.Aeson                             hiding (withArray)
import Data.Maybe                             (fromMaybe)
import Data.Text                              (Text)
import Foreign.C                              
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
import Foreign.Storable                       (sizeOf)
import FRP.Yampa
import Graphics.Rendering.OpenGL as GL hiding (Size, Position, Point, pgeo_positions)
import qualified Data.ByteString.Lazy as B

import SDL                             hiding (Point, Vec2, Vec3, Event)

import NGL.LoadShaders
import Input

type WinInput = Event SDL.EventPayload
type Vec2     = (Double, Double)
type Vec3     = (Double, Double, Double)

data PGeo =
     PGeo
     {
       pgeo_positions :: [Vec3] -- position of vertices positions as Vec3
     , uv             :: [Vec3]
     } deriving Show

-- < NGL (NGL is not a Graphics Library) > --------------------------------

data Projection
   = Planar
   deriving Show

data Shape2D
   = Square Vec2 Size
   deriving Show

newtype Shape3D
  =
    Geo
    {
      positions :: [Vec3]
    }
  deriving Show

-- type Drawable   = [Vertex4 Double]
data Drawable
  =
    Drawable
    {
      verts  :: [Vertex4 Double]
    , uvs    :: [Vertex4 Double]
    }
type Size       = Double

class Vec2Vertex a where
  toVertex4  :: a -> Vertex4 Double
instance Vec2Vertex Vec2 where
  toVertex4 :: Vec2 -> Vertex4 Double
  toVertex4 (k, l)    = Vertex4 k l 0 1
instance Vec2Vertex Vec3 where
  toVertex4 :: Vec3 -> Vertex4 Double
  toVertex4 (k, l, m) = Vertex4 k l m 1

class Drawables a where
  fromShape :: a -> IO Drawable
instance Drawables Shape2D where
  fromShape :: Shape2D -> IO Drawable
  fromShape x = 
    do
      -- * TODO : complete undefined
      let ps  = map toVertex4 $ toVec2s x
          uvs = undefined :: [Vertex4 Double]
      return $ Drawable ps uvs
instance Drawables Shape3D where
  fromShape :: Shape3D -> IO Drawable
  fromShape x =
    do
      let ps  = map toVertex4 $ toVec3s x
          uvs = undefined :: [Vertex4 Double]
      return $ Drawable ps uvs
  
square :: Vec2 -> Double -> [Vec2]
square pos side = [p1, p2, p3,
                   p1, p3, p4]
    where          
        x = fst pos
        y = snd pos
        r = side/2 
        p1 = (x + r, y + r)
        p2 = (x - r, y + r)
        p3 = (x - r, y - r)
        p4 = (x + r, y - r)


liftVec2 :: Vec2 -> Vec3
liftVec2 (x,y) = (x, y, 0.0)

liftVec2s :: [Vec2] -> [Vec3]
liftVec2s = map liftVec2

toVec2s :: Shape2D -> [Vec2]
toVec2s (Square pos side) =  square pos side

toVec3s :: Shape3D -> [Vec3]
toVec3s Geo { positions } = positions

toUV :: Projection -> [TexCoord2 Double]
toUV Planar =
  projectPlanar ps
                  where ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
                             ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)] :: [Vec2]

toTexCoord2 :: (a, a, a) -> TexCoord2 a
toTexCoord2 (k, l, m) = TexCoord2 k l

toTexCoord2s :: [(a, a, a)] -> [TexCoord2 a]
toTexCoord2s = map toTexCoord2 

projectPlanar :: [Vec2] -> [TexCoord2 Double]
projectPlanar      = map $ uncurry TexCoord2
    
-- < Reading PGeo > --------------------------------------------------------

newtype Position = Position [Vec3] deriving Show
newtype UV       = UV       [Vec3] deriving Show

instance FromJSON PGeo where
  parseJSON (Object o) =
     PGeo
       <$> ((o .: "PGeo") >>= (.: "position"))
       <*> ((o .: "PGeo") >>= (.: "uv"))
  parseJSON _ = mzero

instance FromJSON Position where
    parseJSON (Object o) =
      do
        pts <- o .: "position"
        Position <$> parseJSON pts
    parseJSON _ = mzero

instance FromJSON UV where
    parseJSON (Object o) =
      do
        uv <- o .: "uv"
        UV <$> parseJSON uv
    parseJSON _ = mzero

type Positions = [Vertex4 Double] 

data Transform = Transform {}

jsonFile :: FilePath
jsonFile = "src/model.pgeo"           

getJSON :: FilePath -> IO B.ByteString
getJSON  = B.readFile

readPGeo :: IO ([Vec3], [Vec3])
readPGeo =
  do
    d <- (eitherDecode <$> getJSON jsonFile) :: IO (Either String PGeo)
    let positions = (pgeo_positions . fromEitherDecode) d
    let uvs       = (uv             . fromEitherDecode) d
    return ( positions
           , uvs)

      where
        fromEitherDecode = fromMaybe (PGeo [] []) . fromEither
        fromEither d =
          case d of
            Left err -> Nothing
            Right ps -> Just ps
                  

-- < Rendering > ----------------------------------------------------------
openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"  
     
    window <- SDL.createWindow
            "Mandelbrot Yampa / SDL / OpenGL Example"
            SDL.defaultWindow {SDL.windowInitialSize = V2 sizex sizey,
                               SDL.windowOpenGL = Just SDL.defaultOpenGL}
    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window = do
    SDL.destroyWindow window
    SDL.quit


-- * TODO : (Drawable -> Double) is, effectively, a state passing, refactor to more scalable
-- i.e. pass a state data structure instead
draw :: SDL.Window -> Drawable -> Double -> IO ()
draw window drawable offset = do
      (Descriptor triangles firstIndex numVertices) <- initResources drawable offset

      GL.clearColor $= Color4 0 0 0 1
      GL.clear [ColorBuffer]
      bindVertexArrayObject $= Just triangles
      drawArrays Triangles firstIndex numVertices

      SDL.glSwapWindow window

-- < OpenGL > -------------------------------------------------------------
data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices


-- * TODO : same, state passing is clumsy and incomplete
--initResources :: [Vertex4 Double] -> Double -> IO Descriptor
initResources :: Drawable -> Double -> IO Descriptor
initResources dw offset = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    --
    -- Declaring VBO: vertices
    --
    let vs          = verts dw
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

    --let uvs = uv model
    geo <- readPGeo
    let uv  = toTexCoord2s $ (\(_, uv) -> uv) geo

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

    -- Set Uniforms
    location <- get (uniformLocation program "fTime")
    uniform location $= (realToFrac offset :: GLfloat)

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)    

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

-- < Animate > ------------------------------------------------------------

type WinOutput = (Game, Bool)

animate :: Text                   -- ^ window title
        -> CInt                   -- ^ window width in pixels
        -> CInt                   -- ^ window height in pixels
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    window <- openWindow title (winWidth, winHeight)

    lastInteraction <- newMVar =<< SDL.time   
      
    let senseInput _ = do
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent                          
            return (dt, Event . SDL.eventPayload <$> mEvent) 

        renderOutput _ (Game offset _, shouldExit) = do -- | add (winx, winy) and process it similar to (offset)
            -- let ps  = pgeo_positions model
            -- let geo = Geo ps
            geo <- readPGeo
            -- TODO : pass state
            --drawable <- fromShape $ Geo $ (\(ps, _) -> ps) geo
            drawable <- fromShape $ Geo $ (\(ps, _) -> ps) geo
            --draw window ( fromShape $ Geo $ (\(ps, _) -> ps) geo ) offset
            draw window drawable offset
            return shouldExit 

    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf

    closeWindow window

-- < Game Logic > ---------------------------------------------------------
data GameStage = GameIntro
               | GamePlaying
               | GameFinished
               | GameMenu
               deriving Show

data Game =
     Game
     { -- Game State
       pVal :: Double
     -- , pPos  :: Double    -- Player Position
     -- , bPos  :: Pos       -- Ball   Position
     , gStg  :: GameStage -- Game   Stage
     } 
     deriving Show

type Pos  = (Double, Double)

defaultGame :: Game
defaultGame =
  Game pt0 GameIntro
  where
    pt0 = 0 :: Double

mainGame :: SF AppInput Game
mainGame =
  loopPre defaultGame $ 
  proc (input, gameState) -> do
    gs <- case gStg gameState of
            GameIntro   -> gameIntro -< (input, gameState)
            GamePlaying -> gamePlay  -< input
    returnA -< (gs, gs)

gameIntro :: SF (AppInput, Game) Game
gameIntro =
  switch sf cont        
     where sf =
             proc (input, gameState) -> do
               introState <- returnA -< gameState
               playState  <- returnA -< gameState { gStg =  GamePlaying }
               skipE      <- key SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after loadDelay () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) `tag` playState)
           cont game  = 
             proc input -> do
               returnA  -< game

gamePlay :: SF AppInput Game
gamePlay =
    switch sf (const mainGame)        
     where sf =
             proc input -> do
               gameState <- gameSession -< input
               reset     <- key SDL.ScancodeSpace "Pressed" -< input
               returnA   -< (gameState, reset)

playerVal :: Double -> SF AppInput Double
playerVal pp0 =
  switch sf cont
    where
      sf = proc input -> do
        keyLeft  <- key SDL.ScancodeLeft  "Pressed" -< input
        keyRight <- key SDL.ScancodeRight "Pressed" -< input
        let res :: (  Double
                    , Event ()
                    , Event ())
            res =  ( pp0
                   , keyLeft
                   , keyRight)
        returnA -< (pp0, mergeEvents
                         [ keyLeft
                         , keyRight ] `tag` res)

      cont (x, keyLeft, keyRight) =
        if | isEvent keyLeft -> incVal x (-0.5)
           | otherwise       -> incVal x   0.5

incVal :: Double -> Double -> SF AppInput Double
incVal pp0 v0 =
  switch sf cont
    where
         sf = proc input -> do
            p       <- -- DT.trace ("p: " ++ show pp0 ++ "\n") $
                       (pp0 +) ^<< integral -< v0
            keyLeft <- key SDL.ScancodeLeft  "Released" -< input
            keyRight<- key SDL.ScancodeRight "Released" -< input
            returnA -< (p, mergeEvents
                           [ keyLeft  
                           , keyRight ] `tag` p) :: (Double, Event Double)
         cont = playerVal

gameSession :: SF AppInput Game
gameSession = 
  proc input -> do
       pval  <- playerVal $ pVal defaultGame -< input
       returnA      -< Game pval GamePlaying

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

-- < Global Constants > ---------------------------------------------------
mBlur     = 0.25 :: Float
loadDelay = 2.0  :: Double
resX      = 800  :: CInt
resY      = 600  :: CInt

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = 
     animate
       "e1337"
       resX
       resY
       (parseWinInput >>> (mainGame &&& handleExit))

{-
GameState:
    Position : Pos          :: V3, 64bit
             , Time         :: Integer + Float or Double
             , 3xQuaternion :: 3x4, Float
                 (acceleration for pitch, roll, yaw)
             --> 12 values + 4 = 16 -> M4

    Pos0    : Pos
            , Time
            , Orient       :: 2xQuat, 16bit
            --> 4 + 4 = 8
      // for Stellar Bodies:
            , probablity of collision -> collision map (Size -> Prob)

    Planet : Time
           , Civilization

    Civilization : Time
                 , Technology

    Technology : Technology Tree -> a point cloud, representing ideas, which have mass, so that ideas interact with gravity.  Researcers are like particles, circling around, eventually orbiting and colliding with an idea.  Orbitting an idea gives a bonus, hitting an idea gives another bonus and maybe extras, and excludes it from the PC and the sim continues.  Different ideologies or mentalities, defined as social factors, effect the geometry of the idea-space. Thus to some societies certain technologies and other objects from the idea-space take shorter or more trivial paths than to other societies, certain objects may be missing in the idea-space of certain societies and are only obtainable via interaction with other societies, this the idea-spaces are complementrary on the sense of object repertoir, the goemtry remains different. For a society that obtains originally missing idea is like finding a new branch of science, or inserting new objects into an idea-space.

, Gravity Sim to represent scientific effort
  with research units (spheres) orbit each other and collide,
  collision representing a scientific discovery or a breakthrough.
, Physical Params

, Terrain - some sort of evolution, for now can be an Int to define a state variant


Cosmic Body:
  p', Pivot     V3 (center of mass)
  p , Position  V3 + r , Orientation Q4 -> M2x4 (?)
  v , Velocity, V3
  m , Mass      F1
  d , Density,  F1


  a , Acceleration V3
  a', Angular Velocity 2xQuat (rotation speed)
-}
