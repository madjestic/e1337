{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module Main where 

import Control.Concurrent
import Control.Monad
import Data.Text                              (Text)
import Foreign.C                              
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
import Foreign.Storable                       (sizeOf)
import FRP.Yampa                              hiding (identity)
import Graphics.Rendering.OpenGL as GL        hiding (Size, Position, Point, position)
import Linear.Matrix
import SDL                                    hiding (Point, Vec2, Vec3, Event)

import NGL.LoadShaders
import Game
import Geometry
import Input

-- < NGL (NGL is not a Graphics Library) > -------------------------------------

data Drawable
  =
    Drawable
    {
      verts  :: [Vertex4   Double]
    , uvs    :: [TexCoord2 Double]
    }
type Size       = Double

class FromVector a where
  toVertex4  :: a -> Vertex4 Double
instance FromVector Vec2 where
  toVertex4 :: Vec2 -> Vertex4 Double
  toVertex4 (k, l)    = Vertex4 k l 0 1
instance FromVector Vec3 where
  toVertex4 :: Vec3 -> Vertex4 Double
  toVertex4 (k, l, m) = Vertex4 k l m 1

-- < 2D Shapes > -----------------------------------------------------------
data Shape2D
   = Square Vec2 Size
   deriving Show

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

-- < Shapes to Drawable > --------------------------------------------------
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
  
-- < Texturing > ----------------------------------------------------------
data Projection
   = Planar
   deriving Show

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

-- RRRRRR  EEEEEEE NN   NN DDDDD   EEEEEEE RRRRRR  IIIII NN   NN   GGGG  
-- RR   RR EE      NNN  NN DD  DD  EE      RR   RR  III  NNN  NN  GG  GG 
-- RRRRRR  EEEEE   NN N NN DD   DD EEEEE   RRRRRR   III  NN N NN GG      
-- RR  RR  EE      NN  NNN DD   DD EE      RR  RR   III  NN  NNN GG   GG 
-- RR   RR EEEEEEE NN   NN DDDDDD  EEEEEEE RR   RR IIIII NN   NN  GGGGGG 
                  
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

-- TODO : Game -> Camera -> [Object]
render :: Game -> Drawable
render = undefined

-- draw :: SDL.Window -> Drawable -> IO ()
draw :: SDL.Window -> Game -> IO ()
draw window drw =
  do
    (Descriptor triangles firstIndex numVertices) <- initResources drw

    GL.clearColor $= Color4 0 0 0 1
    GL.clear [ColorBuffer]
    bindVertexArrayObject $= Just triangles
    drawArrays Triangles firstIndex numVertices

    SDL.glSwapWindow window


--  .d88888b. 8888888b. 8888888888888b    888 .d8888b. 888      
-- d88P" "Y88b888   Y88b888       8888b   888d88P  Y88b888      
-- 888     888888    888888       88888b  888888    888888      
-- 888     888888   d88P8888888   888Y88b 888888       888      
-- 888     8888888888P" 888       888 Y88b888888  88888888      
-- 888     888888       888       888  Y88888888    888888      
-- Y88b. .d88P888       888       888   Y8888Y88b  d88P888      
--  "Y88888P" 888       8888888888888    Y888 "Y8888P8888888888 

-- < OpenGL > -------------------------------------------------------------
data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

initResources :: Game -> IO Descriptor
initResources game = do
  
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    --
    -- Declaring VBO: vertices
    --
    drawable <- toDrawable (geometry game)
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
    uniform location $= (realToFrac (pVal game) :: GLfloat)

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)    

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


--        d8888888b    8888888888888b     d888       d8888888888888888888888888 
--       d888888888b   888  888  8888b   d8888      d88888    888    888        
--      d88P88888888b  888  888  88888b.d88888     d88P888    888    888        
--     d88P 888888Y88b 888  888  888Y88888P888    d88P 888    888    8888888    
--    d88P  888888 Y88b888  888  888 Y888P 888   d88P  888    888    888        
--   d88P   888888  Y88888  888  888  Y8P  888  d88P   888    888    888        
--  d8888888888888   Y8888  888  888   "   888 d8888888888    888    888        
-- d88P     888888    Y8888888888888       888d88P     888    888    8888888888 

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

        renderOutput _ (game, shouldExit) =
          do
            draw window game -- game => (game -> renderable
            return shouldExit 

    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf

    closeWindow window


--  .d8888b.88888888888    d8888888888888888888888888888b     d888       d8888 .d8888b. 888    8888888888888b    8888888888888 
-- d88P  Y88b   888       d88888    888    888       8888b   d8888      d88888d88P  Y88b888    888  888  8888b   888888        
-- Y88b.        888      d88P888    888    888       88888b.d88888     d88P888888    888888    888  888  88888b  888888        
--  "Y888b.     888     d88P 888    888    8888888   888Y88888P888    d88P 888888       8888888888  888  888Y88b 8888888888    
--     "Y88b.   888    d88P  888    888    888       888 Y888P 888   d88P  888888       888    888  888  888 Y88b888888        
--       "888   888   d88P   888    888    888       888  Y8P  888  d88P   888888    888888    888  888  888  Y88888888        
-- Y88b  d88P   888  d8888888888    888    888       888   "   888 d8888888888Y88b  d88P888    888  888  888   Y8888888        
--  "Y8888P"    888 d88P     888    888    8888888888888       888d88P     888 "Y8888P" 888    8888888888888    Y8888888888888 

-- < Game Logic > ---------------------------------------------------------
type Pos      = (Double, Double)
type Vec2     = (Double, Double)
type Vec3     = (Double, Double, Double)

mainGame :: Game -> SF AppInput Game
mainGame initGame = 
  loopPre initGame $ 
  proc (input, game) -> do
    gs <- case gStg game of
            GameIntro   -> gameIntro   -< (input, game)
            GamePlaying -> gamePlay initGame -< input
    returnA -< (gs, gs)

gameIntro :: SF (AppInput, Game) Game
gameIntro =
  switch sf cont        
     where sf =
             proc (input, game) -> do
               introState <- returnA -< game
               playState  <- returnA -< game { gStg =  GamePlaying }
               skipE      <- key SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after loadDelay () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) `tag` playState)
           cont game  = 
             proc input -> do
               returnA  -< game

gamePlay :: Game -> SF AppInput Game
gamePlay game =
    switch sf (const (mainGame game))        
     where sf =
             proc input -> do
               game <- update game -< input
               reset     <- key SDL.ScancodeSpace "Pressed" -< input
               returnA   -< (game, reset)

updatePVal :: Double -> SF AppInput Double
updatePVal pp0 =
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
        if | isEvent keyLeft -> updatePVal' x (-0.5)
           | otherwise       -> updatePVal' x   0.5

updatePVal' :: Double -> Double -> SF AppInput Double
updatePVal' pp0 v0 =
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
         cont = updatePVal

defM44 :: M44 Double
defM44 = identity

update :: Game -> SF AppInput Game
update game = 
  proc input -> do
       pVal     <- updatePVal $ pVal game -< input
       --pos update
       returnA -< Game pVal (geometry game) (pos game) GamePlaying

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

jsonFile :: FilePath
jsonFile = "src/model.pgeo"

initGame :: IO Game
initGame =
  do
    geo <- readPGeo jsonFile
    let initGame = Game 0.0 geo defM44 GameIntro
    return initGame

--  CCCCC   OOOOO  NN   NN  SSSSS  TTTTTTT   AAA   NN   NNn TTTTTTT  SSSSS  
-- CC    C OO   OO NNN  NN SS        TTT    AAAAA  NNN  NN   TTT   SS      
-- CC      OO   OO NN N NN  SSSSS    TTT   AA   AA NN N NN   TTT    SSSSS  
-- CC    C OO   OO NN  NNN      SS   TTT   AAAAAAA NN  NNN   TTT        SS 
--  CCCCC   OOOO0  NN   NN  SSSSS    TTT   AA   AA NN   NN   TTT    SSSSS  

-- < Global Constants > ---------------------------------------------------
mBlur     = 0.25 :: Float
loadDelay = 2.0  :: Double
resX      = 800  :: CInt
resY      = 600  :: CInt

-- 888b     d888       d88888888888888b    888 
-- 8888b   d8888      d88888  888  8888b   888 
-- 88888b.d88888     d88P888  888  88888b  888 
-- 888Y88888P888    d88P 888  888  888Y88b 888 
-- 888 Y888P 888   d88P  888  888  888 Y88b888 
-- 888  Y8P  888  d88P   888  888  888  Y88888 
-- 888   "   888 d8888888888  888  888   Y8888 
-- 888       888d88P     8888888888888    Y888 

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do
  initState <- initGame
  animate
    "e1337"
    resX
    resY
    (parseWinInput >>> ((mainGame initState) &&& handleExit))
