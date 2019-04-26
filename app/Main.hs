{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where 

import Control.Concurrent
-- import Control.Monad
import Data.Text                              (Text)
import Foreign.C                              
-- import Foreign.Marshal.Array                  (withArray)
-- import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
-- import Foreign.Storable                       (sizeOf)
import FRP.Yampa                              hiding (identity)
-- import Graphics.Rendering.OpenGL as GL        hiding (Size, Position, Point, position)
import Linear.Matrix
import SDL                                    hiding (Point, Vec2, Vec3, M44, Event, (^+^))
-- import SDL.Raw.Video
-- import SDL.Raw.Enum

-- import NGL.LoadShaders
import Game
import Geometry
import Input
import Rendering
-- import Drawables

-- import Debug.Trace as DT

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
            --_ <- DT.trace "Hello!" $ return ()
            --_ <- DT.trace ("Game: " ++ show game) $ return ()
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
               game      <- updateGame game -< input
               reset     <- key SDL.ScancodeSpace "Pressed" -< input
               returnA   -< (game, reset)

updateGame :: Game -> SF AppInput Game
updateGame game = 
  proc input -> do
       obj <- updateObject $ object game -< input
       returnA -< Game GamePlaying obj

updateObject :: Object -> SF AppInput Object
updateObject obj =
  proc input -> do
    sclr    <- updateScalar $ scalar obj -< input
    mtx     <- updateTransform (transform obj) (keys obj) -< input
    --returnA -< Object sclr (geometry obj) (transform obj) (velocity obj) (keys obj)
    returnA -< Object sclr (geometry obj) mtx (velocity obj) (keys obj)

updateTransform :: M44 Double -> Keys -> SF AppInput (M44 Double)
updateTransform mtx0 keys0 =
  -- | foldrWith mtx0 keys - for every key apply a folding transform to mtx0
  -- | in case of keypress event - update the set of keys and call new fold ^
  switch sf cont
    where
      sf = proc input -> do
        -- update transform according to Keys
        trans <- mkTrans mtx0 keys0 -< ()
        --mtx   <- (^.^) ^<< integral -< (mtx0, keys0)
        -- let mtx = undefined
        keysE <- key SDL.ScancodeSpace "Pressed" -< input -- updateKeys keys0 -< input
        let res = ( trans
                  , isEvent $ keysE )
        returnA -< (trans, keysE `tag` res) -- :: (M44 Double, Event (M44 Double))
      cont (x, keys) = undefined

mkTrans :: M44 Double -> Keys -> SF () (M44 Double)
mkTrans = undefined

-- (^.^) :: M44 Double -> Keys -> M44 Double
-- (^.^) m0 keys0 = undefined

-- (^.^) :: (M44 Double, Keys) -> M44 Double
-- (^.^) = undefined

-- -- | TODO : complete the class instance
-- instance VectorSpace (M44 Double) Keys where
--   zeroVector = identity :: M44 Double
  -- | Vector with no magnitude (unit for addition).

--   -- | Multiplication by a scalar.
--   (*^) :: a -> v -> v
--   (*^) x y = undefined

--   -- | Division by a scalar.
--   (^/) :: v -> a -> v
--   v ^/ a = undefined

--   -- | Vector addition
--   (^+^) :: v -> v -> v
--  (^+^) m0 m1 = undefined

  -- | TODO : add composition operator for matrix rotation and matrix adition (matrx/matrix, matrix/vector)

--   -- | Vector subtraction
--   (^-^) :: v -> v -> v
--   v1 ^-^ v2 = v1 ^+^ negateVector v2

--   -- | Vector negation. Addition with a negated vector should be
--   --   same as subtraction.
--   negateVector :: v -> v
--   negateVector v = (-1) *^ v

  -- -- | Dot product (also known as scalar or inner product).
  -- --
  -- -- For two vectors, mathematically represented as @a = a1,a2,...,an@ and @b
  -- -- = b1,b2,...,bn@, the dot product is @a . b = a1*b1 + a2*b2 + ... +
  -- -- an*bn@.
  -- --
  -- -- Some properties are derived from this. The dot product of a vector with
  -- -- itself is the square of its magnitude ('norm'), and the dot product of
  -- -- two orthogonal vectors is zero.
  -- dot :: v -> v -> a
  -- dot = undefined
  
updateKeys :: Keys -> SF AppInput (Keys)
updateKeys keys = undefined

updateTransform' :: M44 Double -> Keys -> SF AppInput (M44 Double)
updateTransform' mtx0 keys0 = undefined

updateScalar :: Double -> SF AppInput Double
updateScalar pp0 =
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
        if | isEvent keyLeft -> updateScalar' x (-0.5)
           | otherwise       -> updateScalar' x   0.5

updateScalar' :: Double -> Double -> SF AppInput Double
updateScalar' pp0 v0 =
  switch sf cont
    where
         sf =
           proc input -> do
             p       <- -- DT.trace ("p: " ++ show pp0 ++ "\n") $
                        (pp0 +) ^<< integral -< v0
             keyLeft <- key SDL.ScancodeLeft  "Released" -< input
             keyRight<- key SDL.ScancodeRight "Released" -< input
             returnA -< (p, mergeEvents
                            [ keyLeft  
                            , keyRight ] `tag` p) :: (Double, Event Double)
         cont = updateScalar

-- updateKeys :: Keys -> SF AppInput (Keys)
-- updateKeys keys = undefined
--   switch sf cont
--     where
--       sf = proc input -> do
--         keyW     <- key SDL.ScancodeW     "Pressed" -< input
--         keyS     <- key SDL.ScancodeS     "Pressed" -< input
--         keyA     <- key SDL.ScancodeA     "Pressed" -< input
--         keyD     <- key SDL.ScancodeD     "Pressed" -< input

--         keyQ     <- key SDL.ScancodeQ     "Pressed" -< input
--         keyE     <- key SDL.ScancodeE     "Pressed" -< input
--         keyZ     <- key SDL.ScancodeZ     "Pressed" -< input
--         keyX     <- key SDL.ScancodeX     "Pressed" -< input
        
--         keyUp    <- key SDL.ScancodeUp    "Pressed" -< input
--         keyDown  <- key SDL.ScancodeDown  "Pressed" -< input
--         keyLeft  <- key SDL.ScancodeLeft  "Pressed" -< input
--         keyRight <- key SDL.ScancodeRight "Pressed" -< input

--         let pressed =
--               ( mtx0
--               , keyW
--               , keyS
--               , keyA
--               , keyD
--               , keyQ
--               , keyE
--               , keyZ
--               , keyX
--               , keyUp
--               , keyDown
--               , keyLeft
--               , keyRight )
--         returnA -< (keys, mergeEvents
--                      [ keyW
--                      , keyS
--                      , keyA
--                      , keyD
--                      , keyQ
--                      , keyE
--                      , keyZ
--                      , keyX
--                      , keyUp
--                      , keyDown
--                      , keyLeft
--                      , keyRight] `tag` pressed)
--       cont (x, keyW, keyS, keyA, keyD, keyQ, keyE, keyZ, keyX, keyUp, keyDown, keyLeft, keyRight) =
--         updateKeys pressed


-- updateKeys :: Keys -> SF AppInput (Keys)
-- updateKeys keys = --undefined
--   switch sf cont
--     where
--       sf = proc input -> do
--         keyW     <- key SDL.ScancodeW     "Pressed" -< input
--         keyS     <- key SDL.ScancodeS     "Pressed" -< input
--         keyA     <- key SDL.ScancodeA     "Pressed" -< input
--         keyD     <- key SDL.ScancodeD     "Pressed" -< input

--         keyQ     <- key SDL.ScancodeQ     "Pressed" -< input
--         keyE     <- key SDL.ScancodeE     "Pressed" -< input
--         keyZ     <- key SDL.ScancodeZ     "Pressed" -< input
--         keyX     <- key SDL.ScancodeX     "Pressed" -< input
        
--         keyUp    <- key SDL.ScancodeUp    "Pressed" -< input
--         keyDown  <- key SDL.ScancodeDown  "Pressed" -< input
--         keyLeft  <- key SDL.ScancodeLeft  "Pressed" -< input
--         keyRight <- key SDL.ScancodeRight "Pressed" -< input

--         let pressed =
--               ( mtx0
--               , keyW
--               , keyS
--               , keyA
--               , keyD
--               , keyQ
--               , keyE
--               , keyZ
--               , keyX
--               , keyUp
--               , keyDown
--               , keyLeft
--               , keyRight )
--         returnA -< (keys, mergeEvents
--                      [ keyW
--                      , keyS
--                      , keyA
--                      , keyD
--                      , keyQ
--                      , keyE
--                      , keyZ
--                      , keyX
--                      , keyUp
--                      , keyDown
--                      , keyLeft
--                      , keyRight] `tag` pressed)
--       cont (x, keyW, keyS, keyA, keyD, keyQ, keyE, keyZ, keyX, keyUp, keyDown, keyLeft, keyRight) =
--         updateKeys pressed
              

        

updateTransformMatrix :: M44 Double -> V4 Double -> SF AppInput (M44 Double)
updateTransformMatrix mtx0 vel0 = undefined
  switch sf cont
    where
      sf = proc input -> do
        keyW     <- key SDL.ScancodeW     "Pressed" -< input
        keyS     <- key SDL.ScancodeS     "Pressed" -< input
        keyA     <- key SDL.ScancodeA     "Pressed" -< input
        keyD     <- key SDL.ScancodeD     "Pressed" -< input

        keyQ     <- key SDL.ScancodeQ     "Pressed" -< input
        keyE     <- key SDL.ScancodeE     "Pressed" -< input
        keyZ     <- key SDL.ScancodeZ     "Pressed" -< input
        keyX     <- key SDL.ScancodeX     "Pressed" -< input
        
        keyUp    <- key SDL.ScancodeUp    "Pressed" -< input
        keyDown  <- key SDL.ScancodeDown  "Pressed" -< input
        keyLeft  <- key SDL.ScancodeLeft  "Pressed" -< input
        keyRight <- key SDL.ScancodeRight "Pressed" -< input

        let pressed =
              ( mtx0
              , keyW
              , keyS
              , keyA
              , keyD
              , keyQ
              , keyE
              , keyZ
              , keyX
              , keyUp
              , keyDown
              , keyLeft
              , keyRight )
        returnA -< (mtx0, mergeEvents
                          [ keyW
                          , keyS
                          , keyA
                          , keyD
                          , keyQ
                          , keyE
                          , keyZ
                          , keyX
                          , keyUp
                          , keyDown
                          , keyLeft
                          , keyRight] `tag` pressed)

      cont (x, keyW, keyS, keyA, keyD, keyQ, keyE, keyZ, keyX, keyUp, keyDown, keyLeft, keyRight) =
        if | isEvent keyW -> translateMatrix x (V4 ( 0.0) ( 0.0) ( 0.1) ( 0.0)) -- forwards
           | isEvent keyS -> translateMatrix x (V4 ( 0.0) ( 0.0) (-0.1) ( 0.0)) -- backwards
           | isEvent keyA -> translateMatrix x (V4 (-0.1) ( 0.0) ( 0.0) ( 0.0)) -- str.left
           | isEvent keyD -> translateMatrix x (V4 ( 0.1) ( 0.0) ( 0.0) ( 0.0)) -- str.right
           | isEvent keyZ -> translateMatrix x (V4 ( 0.0) ( 0.1) ( 0.0) ( 0.0)) -- str.up
           | isEvent keyX -> translateMatrix x (V4 ( 0.0) (-0.1) ( 0.0) ( 0.0)) -- str.down
           | isEvent keyQ     -> rotateMatrix x (Quaternion ( 0.1) (V3 ( 0.0) ( 0.0) ( 1.0))) -- roll left
           | isEvent keyE     -> rotateMatrix x (Quaternion (-0.1) (V3 ( 0.0) ( 0.0) ( 1.0))) -- roll right
           | isEvent keyUp    -> rotateMatrix x (Quaternion (-0.1) (V3 ( 1.0) ( 0.0) ( 0.0))) -- pitch up
           | isEvent keyDown  -> rotateMatrix x (Quaternion ( 0.1) (V3 ( 1.0) ( 0.0) ( 0.0))) -- pitch down
           | isEvent keyLeft  -> rotateMatrix x (Quaternion ( 0.1) (V3 ( 0.0) ( 1.0) ( 0.0))) -- yaw left
           | isEvent keyRight -> rotateMatrix x (Quaternion (-0.1) (V3 ( 0.0) ( 1.0) ( 0.0))) -- yaw right
           | otherwise    -> translateMatrix x (V4 ( 0.0) ( 0.0) ( 0.0) ( 0.0)) -- nothing

translateMatrix :: M44 Double -> V4 Double -> SF AppInput (M44 Double)
translateMatrix mtx0 vel0 = undefined
  -- switch sf cont
  --   where
  --        sf =
  --          proc input -> do
  --            -- M44 -> V4 tr0 -> Quaternion rot0 -> M44
  --            -- M44 -> _m33 M44 + translation M44 -> (M33 + tr) -> M33 
  --            --                                                 -> tr
                
  --            mtx <- (mtx0 +) ^<< integral -< (\(V4 x y z w) -> V3 x y z) vel0

rotateMatrix :: M44 Double -> Quaternion Double -> SF AppInput (M44 Double)
rotateMatrix = undefined             
             
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

jsonFile :: FilePath
jsonFile = "src/model.pgeo"

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

initGame :: IO Game
initGame =
  do
    geo <- readPGeo jsonFile
    --_ <- DT.trace ("Game: " ++ show geo) $ return ()
    let obj = Object 0.0 geo (identity::M44 Double) (V4 0 0 0 0) (Keys False False False False False False False False False False False False)
    let initGame = Game GamePlaying obj
    return initGame


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
