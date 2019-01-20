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
import Graphics.Rendering.OpenGL as GL hiding (Size, Position, Point, position)
import qualified Data.ByteString.Lazy as B

import SDL                             hiding (Point, Vec2, Vec3, Event)

import NGL.LoadShaders
import Input

-- < NGL (NGL is not a Graphics Library) > --------------------------------

data Projection
   = Planar
   deriving Show

data Shape2D
   = Square Vec2 Size
   deriving Show

data Geo
  =
    Geo
    {
      position :: [Vec3]
    , uv       :: [Vec3]
    }
  deriving Show

data Drawable
  =
    Drawable
    {
      verts  :: [Vertex4   Double]
    , uvs    :: [TexCoord2 Double]
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
  toDrawable :: a -> IO Drawable
instance Drawables Shape2D where
  toDrawable :: Shape2D -> IO Drawable
  toDrawable x = 
    do
      let ps  = map toVertex4 $ (\ (Square pos side)-> square pos side) x
          uvs = toUV Planar
      return $ Drawable ps uvs
instance Drawables Geo where
  toDrawable :: Geo -> IO Drawable
  toDrawable geo =
    do
      let ps  = map toVertex4   $ position geo
          uvs = map toTexCoord2 $ uv geo
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
    
-- < Reading Geo > --------------------------------------------------------

newtype Position = Position [Vec3] deriving Show
newtype UV       = UV       [Vec3] deriving Show

instance FromJSON Geo where
  parseJSON (Object o) =
     Geo
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

readPGeo :: FilePath -> IO Geo
readPGeo jsonFile =
  do
    d <- (eitherDecode <$> getJSON jsonFile) :: IO (Either String Geo)
    let ps        = (position . fromEitherDecode) d
    let uvs       = (uv       . fromEitherDecode) d
    return $ Geo ps uvs

      where
        fromEitherDecode = fromMaybe (Geo [] []) . fromEither
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
    let uv  = uvs dw

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

type WinInput = Event SDL.EventPayload
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

        renderOutput _ (Game offset geo _, shouldExit) = do -- | add (winx, winy) and process it similar to (offset)
            drawable <- toDrawable geo
            draw window drawable offset
            return shouldExit 

    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf

    closeWindow window

--fromGeo :: a -> 
      
-- < Game Logic > ---------------------------------------------------------
type Vec2     = (Double, Double)
type Vec3     = (Double, Double, Double)

data GameStage = GameIntro
               | GamePlaying
               | GameFinished
               | GameMenu
               deriving Show

data Game =
     Game
     { -- Game State
       pVal     :: Double
     , geometry :: Geo
     -- , pPos  :: Double    -- Player Position
     -- , bPos  :: Pos       -- Ball   Position
     , gStg  :: GameStage -- Game   Stage
     } 
     deriving Show

type Pos  = (Double, Double)


mainGame :: Game -> SF AppInput Game
mainGame initGame = 
  loopPre initGame $ 
  proc (input, gameState) -> do
    gs <- case gStg gameState of
            GameIntro   -> gameIntro   -< (input, gameState)
            GamePlaying -> gamePlay initGame -< input
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

gamePlay :: Game -> SF AppInput Game
gamePlay gameState =
    switch sf (const (mainGame gameState))        
     where sf =
             proc input -> do
               gameState <- gameSession gameState -< input
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

gameSession :: Game -> SF AppInput Game
gameSession gameState = 
  proc input -> do
       pval  <- playerVal $ pVal gameState -< input
       returnA -< Game pval (geometry gameState) GamePlaying

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

initGame :: IO Game
initGame =
  do
    geo <- readPGeo jsonFile
    let initGame = Game 0.0 geo GameIntro
    return initGame

-- < Global Constants > ---------------------------------------------------
mBlur     = 0.25 :: Float
loadDelay = 2.0  :: Double
resX      = 800  :: CInt
resY      = 600  :: CInt

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do
  initState <- initGame
  animate
    "e1337"
    resX
    resY
    (parseWinInput >>> ((mainGame initState) &&& handleExit))

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
