-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
-- {-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main where 

-- import Data.Foldable as DF       (toList)
-- import GHC.Generics
import Control.Concurrent
import Control.Lens       hiding (transform, indexed)
import Data.Text                 (Text)
import Foreign.C
import FRP.Yampa          hiding (identity)
import Data.Functor              (($>))

-- import Linear.Matrix      (M44, M33, identity)
import SDL                hiding ( Point
                                 , M44
                                 , M33
                                 , Event
                                 , Mouse
                                 , (^+^)
                                 , (*^))
-- import Data.Aeson               (decodeFileStrict)
import Data.Text                (pack)
import System.Environment       (getArgs)
       
import Camera         as Cam
import Game
import Project 
import Project.Parser
import Keyboard
import Object         --as Obj
import Controllable  
import Geometry
import Input          as Inp
import Rendering
import Material
import Descriptor

import Unsafe.Coerce

-- -- Tests
-- import Data.List.Split (chunksOf)
-- import Data.List.Index (indexed)

import Debug.Trace   as DT

-- --        d8888888b    8888888888888b     d888       d8888888888888888888888888 
-- --       d888888888b   888  888  8888b   d8888      d88888    888    888        
-- --      d88P88888888b  888  888  88888b.d88888     d88P888    888    888        
-- --     d88P 888888Y88b 888  888  888Y88888P888    d88P 888    888    8888888    
-- --    d88P  888888 Y88b888  888  888 Y888P 888   d88P  888    888    888        
-- --   d88P   888888  Y88888  888  888  Y8P  888  d88P   888    888    888        
-- --  d8888888888888   Y8888  888  888   "   888 d8888888888    888    888        
-- -- d88P     888888    Y8888888888888       888d88P     888    888    8888888888 

-- -- < Animate > ------------------------------------------------------------
type WinInput = Event SDL.EventPayload
type WinOutput = (Game, Bool)

animate :: SDL.Window
        -> Descriptor
        -> Game --[[Descriptor]]
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
--animate window ds sf =
animate window ds game' sf =
  do
    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf
    closeWindow window
    
      where
        senseInput _ =
          do
            lastInteraction <- newMVar =<< SDL.time
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent                          
            return (dt, Event . SDL.eventPayload <$> mEvent) 

        renderOutput _ (game, shouldExit) =
          do
            uniforms <- initUniforms game
            -- ds = [[(Descriptor, Material)]]
            -- draw :: Window -> [[(Descriptor, Material)]] -> IO ()
            --let ds = toListOf (objects . traverse . descriptors ) game'
            --_ <- DT.trace ("ds: " ++ show ((ds!!0)!!0)) $ return ()
            --let objs = toListOf objects game'
            --_ <- DT.trace ("objs: " ++ show (objs)) $ return ()
            -- ds' <- initVAO'
            --_ <- DT.trace ("ds': " ++ show (ds')) $ return ()
            -- --draw window ((ds!!0)!!0)
            draw window ds
            --draw window ds
            --mapM (draw window) (concat ds)
            -- SDL.glSwapWindow window
            return shouldExit


-- --  .d8888b.88888888888    d8888888888888888888888888888b     d888       d8888 .d8888b. 888    8888888888888b    8888888888888 
-- -- d88P  Y88b   888       d88888    888    888       8888b   d8888      d88888d88P  Y88b888    888  888  8888b   888888        
-- -- Y88b.        888      d88P888    888    888       88888b.d88888     d88P888888    888888    888  888  88888b  888888        
-- --  "Y888b.     888     d88P 888    888    8888888   888Y88888P888    d88P 888888       8888888888  888  888Y88b 8888888888    
-- --     "Y88b.   888    d88P  888    888    888       888 Y888P 888   d88P  888888       888    888  888  888 Y88b888888        
-- --       "888   888   d88P   888    888    888       888  Y8P  888  d88P   888888    888888    888  888  888  Y88888888        
-- -- Y88b  d88P   888  d8888888888    888    888       888   "   888 d8888888888Y88b  d88P888    888  888  888   Y8888888        
-- --  "Y8888P"    888 d88P     888    888    8888888888888       888d88P     888 "Y8888P" 888    8888888888888    Y8888888888888 

-- -- < Init Game > ----------------------------------------------------------

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = map

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

initObjects :: Project -> IO [Object]
initObjects project = 
  do
    -- (PGeo _ _ _ _ _ _ matPaths) <- readPGeo $ path ((models $ project)!!0)
    (VGeo idxs st vaos matPaths) <- readVGeo $ path ((models project)!!0)
    mats                         <- mapM readMaterial matPaths
    --ds <- initVAO
    --mapM initVAO 
    -- VGeo -> [Descriptors]
    let args = (\idx' st' vao' mat' ->  (idx', st', vao', mat')) <$.> idxs <*.> st <*.> vaos <*.> mats
    ds <- mapM initVAO args
    --print $ "args :" ++ show args
    let objects = 
          (fmap (\modelPath -> defaultObj { _descriptors = ds --geoPath = modelPath -- TODO: add descriptor initialize here
                                          , _materials   = mats })
            $ (fmap path) . models $ project :: [Object])
    --let result = undefined
    return objects

initGame :: Project -> IO Game
initGame project =
  do
    -- print d
    objs <- (initObjects project)
    let initGame =
          Game
          ( Options
            name'
            resX'
            resY'
          )
          GamePlaying
          objs
          initCam
    return initGame
      where
        name'           = Project.name  $ project
        resX'           = (unsafeCoerce $ Project.resx $ project) :: CInt
        resY'           = (unsafeCoerce $ Project.resy $ project) :: CInt

-- < Game Logic > ---------------------------------------------------------

-- mainGame :: Game -> SF AppInput Game
-- mainGame suka =
--   proc (input, game) -> do
--     returnA -< undefined


mainGame :: Game -> SF AppInput Game
mainGame initGame =
  loopPre initGame $ 
  proc (input, game) -> do
    gs <- case _gStg game of
            GameIntro   -> gameIntro   -< (input, game)
            GamePlaying -> gamePlay initGame -< input
    returnA -< (gs, gs)

gameIntro :: SF (AppInput, Game) Game
gameIntro =
  switch sf cont        
     where sf =
             proc (input, game) -> do
               introState <- returnA -< game
               playState  <- returnA -< game { _gStg =  GamePlaying }
               skipE      <- keyInput SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after loadDelay () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) $> playState)
           cont game  = 
             proc input -> returnA -< game

gamePlay :: Game -> SF AppInput Game
gamePlay game =
    switch sf (const (mainGame game))        
     where sf =
             proc input -> do
               game      <- updateGame game -< input
               reset     <- keyInput SDL.ScancodeSpace "Pressed" -< input
               returnA   -< (game, reset)

updateGame :: Game -> SF AppInput Game
updateGame game = 
  proc input -> do
    let objs = _objects game
    cam      <- updateCamera $ _camera game -< input
    --returnA  -< Game (_options game) GamePlaying objs cam
    returnA  -< Game (view options game) GamePlaying (view objects game) cam

updateCamera :: Camera -> SF AppInput Camera
updateCamera cam = 
  proc input -> do
    ctl <- loopControlable (Cam.controller $ cam) -< input
    returnA -< Camera { Cam.controller = ctl }

loopControlable :: Controllable -> SF AppInput Controllable
loopControlable ctl0 =
  -- | foldrWith mtx0 keys - for every keyInput apply a folding transform to mtx0
  -- | in case of keypress event - updateControllable the set of keys and call new fold ^
  switch sf cont
    where
      sf = proc input -> do
        ctl           <- updateControllable ctl0       -< ctl0
        mtx           <- returnA          -< transform ctl
        ypr           <- returnA          -< ypr       ctl
        (kkeys, kevs) <- updateKeys  ctl0 -< input
        (pos0, mev)   <- (mouseEventPos &&& mouseEvent) -< input

        result <-
          returnA -<
          ( Controllable (0,0) mtx ypr
            ( Devices
              ( Keyboard kkeys (keyVecs (keyboard (devices ctl))) )
              ( Mouse Nothing Nothing pos0 [] ) ) )
        returnA -< 
          ( result
          , catEvents (mev:kevs)
            $> result) -- :: (Controllable, Event Controllable)
      cont result = loopControlable result

updateKeys :: Controllable -> SF AppInput (Keys, [Event ()])
updateKeys ctl0 = 
  proc input -> do
    let keys0 = keys.keyboard.devices $ ctl0
    
    (keyW_, keyWe) <- keyEvents SDL.ScancodeW keyW ctl0 -< input
    (keyS_, keySe) <- keyEvents SDL.ScancodeS keyS ctl0 -< input
    (keyA_, keyAe) <- keyEvents SDL.ScancodeA keyA ctl0 -< input
    (keyD_, keyDe) <- keyEvents SDL.ScancodeD keyD ctl0 -< input

    (keyQ_, keyQe) <- keyEvents SDL.ScancodeQ keyQ ctl0 -< input
    (keyE_, keyEe) <- keyEvents SDL.ScancodeE keyE ctl0 -< input
    (keyZ_, keyZe) <- keyEvents SDL.ScancodeZ keyZ ctl0 -< input
    (keyX_, keyXe) <- keyEvents SDL.ScancodeX keyX ctl0 -< input
  
    (keyUp_,    keyUpE)    <- keyEvents SDL.ScancodeUp    keyUp    ctl0 -< input
    (keyDown_,  keyDownE)  <- keyEvents SDL.ScancodeDown  keyDown  ctl0 -< input
    (keyLeft_,  keyLeftE)  <- keyEvents SDL.ScancodeLeft  keyLeft  ctl0 -< input
    (keyRight_, keyRightE) <- keyEvents SDL.ScancodeRight keyRight ctl0 -< input

    let events = [      keyWe, keySe, keyAe, keyDe, keyQe, keyEe, keyZe, keyXe, keyUpE, keyDownE, keyLeftE, keyRightE  ]
        keys   = ( Keys keyW_  keyS_  keyA_  keyD_  keyQ_  keyE_  keyZ_  keyX_  keyUp_  keyDown_  keyLeft_  keyRight_ )

    returnA -< (keys, events)

updateControllable :: Controllable -> SF Controllable Controllable
updateControllable ctl0 =
  iterFrom update1 ctl0
  where
    update1 :: Controllable -> Controllable -> DTime -> Controllable -> Controllable
    update1 ctl0 ctl1 dt ctl2 = ctl
      where
        kvs = (keyVecs.keyboard.devices $ ctl0)
        mvs = [ -- mouse vectors
                (V3 )]
        pos0 = pos.mouse.devices $ ctl2

        ctl = (Controllable
               (0,0)
               mtx
               ypr
               (Devices
                (Keyboard keys0 kvs)
                (Mouse Nothing Nothing pos0 [])))
          where
            mtx0  = (transform ctl2)
            keys0 = (keys.keyboard.devices $ ctl0)

            ypr :: V3 Double
            ypr   =
              (99999 * ) $
              ((V3 dt dt dt) * ) $
              foldr (+) (V3 0 0 0) $
              zipWith (*^) ((\x -> if x then 1 else 0) . ($ keys0) <$>
                            [ keyUp,  keyDown, keyLeft, keyRight, keyQ,  keyE ])
                            [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
              where
                pPitch = (keyVecs.keyboard.devices $ ctl0)!!6  -- positive  pitch
                nPitch = (keyVecs.keyboard.devices $ ctl0)!!7  -- negative  pitch
                pYaw   = (keyVecs.keyboard.devices $ ctl0)!!8  -- positive  yaw
                nYaw   = (keyVecs.keyboard.devices $ ctl0)!!9  -- negative  yaw
                pRoll  = (keyVecs.keyboard.devices $ ctl0)!!10 -- positive  roll
                nRoll  = (keyVecs.keyboard.devices $ ctl0)!!11 -- negative  roll
            
            mtx =
              mkTransformationMat
              rot
              tr
              where
                rot =
                  (view _m33 mtx0)
                  !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr)) -- yaw
                  !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr)) -- pitch
                  !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr)) -- roll

                tr  =
                  foldr (+) (view translation mtx0) $
                  fmap (0.1 *) $
                  fmap (transpose (rot) !*) $
                  zipWith (*^) ((\x -> if x then 1 else 0) . ($ keys0) <$>
                                [keyW, keyS, keyA, keyD, keyZ, keyX])
                                [fVel, bVel, lVel, rVel, uVel, dVel]

                  where fVel   = (keyVecs.keyboard.devices $ ctl0)!!0  -- forwards  velocity
                        bVel   = (keyVecs.keyboard.devices $ ctl0)!!1  -- backwards velocity
                        lVel   = (keyVecs.keyboard.devices $ ctl0)!!2  -- left      velocity
                        rVel   = (keyVecs.keyboard.devices $ ctl0)!!3  -- right     velocity
                        uVel   = (keyVecs.keyboard.devices $ ctl0)!!4  -- right     velocity
                        dVel   = (keyVecs.keyboard.devices $ ctl0)!!5  -- right     velocity

keyEvents :: SDL.Scancode -> (Keys -> Bool) -> Controllable -> SF AppInput (Bool, Event ())
keyEvents code keyFunc ctl = 
  proc input -> do
    keyPressed     <- keyInput code  "Pressed"  -< input
    keyReleased    <- keyInput code  "Released" -< input
    let keys0  = keys.keyboard.devices $ ctl
        result = keyEvent (keyFunc keys0) keyPressed keyReleased
        event  = lMerge keyPressed keyReleased
    returnA -< (result, event)

keyEvent :: Bool -> Event () -> Event () -> Bool
keyEvent state pressed released
  | isEvent pressed  = True
  | isEvent released = False
  | otherwise = state

instance VectorSpace (V3 Double) Double where
  zeroVector                   = (V3 0 0 0)
  (*^) s (V3 x y z)            = (V3 (s*x) (s*y) (s*z))
  (^+^)  (V3 x y z) (V3 k l m) = (V3 (x+k) (y+l) (z+m))
  dot    (V3 x y z) (V3 k l m) = (x*k) + (y*l) + (z*m)

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
        
-- -- < Global Constants > --------------------------------------------------------
mBlur     = 0.25 :: Float
loadDelay = 2.0  :: Double

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  proj <- Project.Parser.parse (unsafeCoerce (args!!0) :: FilePath)
  game <- initGame proj -- =<< Project.Parser.parse (unsafeCoerce (args!!0) :: FilePath)

  let title = pack $ view (options . Game.name) game --((pack $ Game._name . _options $ game) :: Text)
      resX  = view (options . Game.resx) game --(_resx . _options $ game)
      resY  = view (options . Game.resx) game --(_resy . _options $ game)

  window    <- openWindow
               title
               (resX, resY)

  -- print game
  -- print $ view (options . Game.name) game
  -- print $ toListOf (objects . traverse . scalar) game
  -- print $ toListOf (objects . traverse . Object.materials . traverse . Material.name) game
  -- let ds = toListOf (objects . traverse . descriptors ) game
  
  -- print ds
  -- let ds = gameDescriptors game :: [[Descriptor]] -- TODO: Game -> Objects -> [[Descriptor]]
  ds <- initVAO'
  
  animate
    window
    ds
    game
    (parseWinInput >>> (mainGame game &&& handleExit))

gameDescriptors :: Game -> [[Descriptor]]
gameDescriptors game = undefined
