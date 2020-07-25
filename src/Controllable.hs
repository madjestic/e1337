{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

module Controllable
  ( Controllable (..)
  , Device (..)
  , Keyboard (..)
  , Mouse (..)
  , transform
  , transform'
  , ypr      
  , device
  , device'
  , mouse
  , keyboard
  , updateController
  ) where

import Linear.Matrix
import Linear.V3
import Linear.V4
import Linear.Quaternion

import Control.Lens hiding (transform)
import Control.Lens.TH
import FRP.Yampa --(SF, Event, returnA, isEvent, lMerge, catEvents, )
import SDL.Input.Keyboard.Codes as SDL
import Data.Functor              (($>))

import Keyboard
import Mouse
import AppInput
import Utils
--import Solvable

data Controllable
  =  Controller
     {
       _debug      :: (Double, Double)
     , _transform  :: M44 Double
     , _ypr        :: V3 Double  -- yaw/pitch/roll
     , _device     :: Device
     }
  |  Solver
     {
--       _pivot      :: V3 Double
       _transform  :: M44 Double
     , _ypr        :: V3 Double  -- yaw/pitch/roll
--     , _velocity   :: V3 Double
--     , _physC      :: Physics -- TODO : add phys.parms
     }
  deriving Show

data Device
  =  Device
     {
       _keyboard :: Keyboard 
     , _mouse    :: Mouse    
     } deriving Show

transform' :: Lens' Controllable (M44 Double)
transform' = lens _transform (\controllable newTransform -> Solver { _transform = newTransform })

device'    :: Lens' Controllable Device
device'    = lens _device    (\controllable newDevice    -> Controller { _device    = newDevice })

$(makeLenses ''Device)
$(makeLenses ''Controllable)

-- | foldrWith mtx0 keys - for every keyInput apply a folding transform to mtx0
-- | in case of keypress event - updateController' the set of keys and call new fold ^  
updateController :: Controllable -> SF AppInput Controllable
updateController ctl0 =
  proc input -> do
    (mrx,mry) <- mouseRelPos -< input
    --ctl1      <- updateKeyboard (DT.trace ("ctl0 :" ++ show ctl0) $ ctl0) -< input
    ctl1      <- updateKeyboard ctl0 -< input
    let
      keys1 = (keys._keyboard._device $ ctl1)
      ypr1 :: V3 Double
      ypr1  =
        -- (1 * ) $
        -- (((0.25 * ) (V3 dt dt dt)) * ) $
        (((1500 * ) (V3 mry mrx 0.0)) + ) $
        (15000 *) $
        foldr1 (+) $
        zipWith (*^) ((\x -> if x then (1.0::Double) else 0) . ($ keys1) <$>
                      [ keyUp,  keyDown, keyLeft, keyRight, keyQ,  keyE ])
                      [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
    
        where
          pPitch = (keyVecs._keyboard._device $ ctl0)!!6  -- positive  pitch
          nPitch = (keyVecs._keyboard._device $ ctl0)!!7  -- negative  pitch
          pYaw   = (keyVecs._keyboard._device $ ctl0)!!8  -- positive  yaw
          nYaw   = (keyVecs._keyboard._device $ ctl0)!!9  -- negative  yaw
          pRoll  = (keyVecs._keyboard._device $ ctl0)!!10 -- positive  roll
          nRoll  = (keyVecs._keyboard._device $ ctl0)!!11 -- negative  roll

    --ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< (DT.trace ("ypr1 :" ++ show ypr1) $ ypr1)
    ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr1
    
    let
      mtx0 = view Controllable.transform ctl0
      mtx1 = view Controllable.transform ctl1
      rot = -- M33
        (view _m33 mtx0)
            -- !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x (DT.trace ("ypr' :" ++ show ypr') $ ypr'))) -- yaw
            -- !*! fromQuaternion (axisAngle (V3 1 0 0) (view _x ypr'))-- (view _x (DT.trace ("ypr' :" ++ show ypr') $ ypr'))) -- yaw-- (view _x ypr')) -- yaw

            -- These 2 seem to be identical, which probably means that it's not right...
            -- !*! fromQuaternion (axisAngle (V3 1 0 0) (view _x ypr')) -- yaw
            -- !*! fromQuaternion (axisAngle (V3 0 1 0) (view _y ypr')) -- pitch
            -- !*! fromQuaternion (axisAngle (V3 0 0 1) (view _z ypr')) -- roll

            !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
            !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
            !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll

      
      tr1  = -- V3
        --foldr (+) (view translation mtx0) $
        --foldr (+) (view translation (view Controllable.transform ctl1)) $
        foldr1 (+) $
        fmap (5000000 *) $ -- <- make it keyboard controllabe: speed up/down
        fmap (transpose (rot) !*) $
        zipWith (*^) ((\x -> if x then (1::Double) else 0) . ($ keys1) <$>
                      [keyW, keyS, keyA, keyD, keyZ, keyC])
                      [fVel, bVel, lVel, rVel, uVel, dVel]

        where fVel   = (keyVecs._keyboard._device $ ctl0)!!0  -- forwards  velocity
              bVel   = (keyVecs._keyboard._device $ ctl0)!!1  -- backwards velocity
              lVel   = (keyVecs._keyboard._device $ ctl0)!!2  -- left      velocity
              rVel   = (keyVecs._keyboard._device $ ctl0)!!3  -- right     velocity
              uVel   = (keyVecs._keyboard._device $ ctl0)!!4  -- right     velocity
              dVel   = (keyVecs._keyboard._device $ ctl0)!!5  -- right     velocity
    
    tr'  <- ((view translation (Controllable._transform ctl0)) ^+^) ^<< integral -< tr1
    let
      mtx = 
        mkTransformationMat
        --(DT.trace ("rot :" ++ show rot) $ rot)
        rot
        tr'
        where -- TODO: Move into a separate function?, same for ypr?             

      --ctl = ctl1 { Controllable._transform = (DT.trace ("mtx :" ++ show mtx) $ mtx)
      ctl = ctl1 { Controllable._transform = mtx
                 , Controllable._ypr = V3 0 0 0 }

    --returnA -< (DT.trace ("ctl :" ++ show ctl) $ ctl)
    returnA -< ctl

updateKeyboard :: Controllable -> SF AppInput Controllable
updateKeyboard ctl0 =
  switch sf cont
  where
    sf =
      proc input -> do
        (kkeys,  kevs) <- updateKeys  ctl0 -< input
        let
          result = ctl0
                   { _device =
                       ( _device ctl0 )
                       { _keyboard =
                           (_keyboard._device $ ctl0)
                           { keys = kkeys} } }
          
        returnA -<
          ( ctl0
          , catEvents kevs $> result )

    cont c = updateKeyboard c

updateKeys :: Controllable -> SF AppInput (Keys, [Event ()])
updateKeys ctl0 = 
  proc input -> do
    (keyW_, keyWe) <- keyEvent SDL.ScancodeW keyW ctl0 -< input
    (keyS_, keySe) <- keyEvent SDL.ScancodeS keyS ctl0 -< input
    (keyA_, keyAe) <- keyEvent SDL.ScancodeA keyA ctl0 -< input
    (keyD_, keyDe) <- keyEvent SDL.ScancodeD keyD ctl0 -< input

    (keyQ_, keyQe) <- keyEvent SDL.ScancodeQ keyQ ctl0 -< input
    (keyE_, keyEe) <- keyEvent SDL.ScancodeE keyE ctl0 -< input
    (keyZ_, keyZe) <- keyEvent SDL.ScancodeZ keyZ ctl0 -< input
    (keyC_, keyCe) <- keyEvent SDL.ScancodeC keyC ctl0 -< input
    (keyPageUp_,   keyPageUpE)   <- keyEvent SDL.ScancodePageUp   keyPageUp   ctl0 -< input
    (keyPageDown_, keyPageDownE) <- keyEvent SDL.ScancodePageDown keyPageDown ctl0 -< input

    (keyLShift_, keyLShiftE) <- keyEvent SDL.ScancodeLShift keyLShift ctl0 -< input
    (keyLCtrl_ , keyLCtrlE)  <- keyEvent SDL.ScancodeLCtrl  keyLCtrl  ctl0 -< input
  
    (keyUp_,    keyUpE)    <- keyEvent SDL.ScancodeUp    keyUp    ctl0 -< input
    (keyDown_,  keyDownE)  <- keyEvent SDL.ScancodeDown  keyDown  ctl0 -< input
    (keyLeft_,  keyLeftE)  <- keyEvent SDL.ScancodeLeft  keyLeft  ctl0 -< input
    (keyRight_, keyRightE) <- keyEvent SDL.ScancodeRight keyRight ctl0 -< input

    let events = [      keyWe, keySe, keyAe, keyDe, keyQe, keyEe, keyZe, keyCe, keyUpE, keyDownE, keyLeftE,   keyRightE,    keyPageUpE, keyPageDownE, keyLShiftE, keyLCtrlE ]
        keys   = ( Keys keyW_  keyS_  keyA_  keyD_  keyQ_  keyE_  keyZ_  keyC_  keyUp_  keyDown_  keyLeft_    keyRight_     keyPageUp_  keyPageDown_  keyLShift_  keyLCtrl_ )

    returnA -< (keys, events)

keyEvent :: SDL.Scancode -> (Keys -> Bool) -> Controllable -> SF AppInput (Bool, Event ())
keyEvent code keyFunc ctl = 
  proc input -> do
    keyPressed     <- keyInput code  "Pressed"  -< input
    keyReleased    <- keyInput code  "Released" -< input
    let
      keys0  = keys._keyboard._device $ ctl
      result = keyState (keyFunc keys0) keyPressed keyReleased
      event  = lMerge keyPressed keyReleased
    returnA -< (result, event)

keyState :: Bool -> Event () -> Event () -> Bool
keyState state pressed released
  | isEvent pressed  = True
  | isEvent released = False
  | otherwise        = state

updateMouse :: Controllable -> SF AppInput (Mouse, [Event ()])
updateMouse ctl0 = 
  proc input -> do
-- TODO: Should be something along the lines with:
-- _ <- mouseEvents -< input
    mouseMovedE   <- mouseMovedEvent   ctl0 -< input -- relative motion event
    mouseStoppedE <- mouseStoppedEvent ctl0 -< input
    rpos <- mouseRelPos -< input
--    pos <- 
    let
      --event  = mouseMovedE $> ():: Event ()
      events = [(mouseMovedE $> ())
               ,(mouseStoppedE $> ())]
      mouse  =
        Mouse
        (lmb._mouse._device $ ctl0)
        (rmb._mouse._device $ ctl0)
        (_pos._mouse._device $ ctl0)
        ((\e -> if isEvent e then fromEvent e else (0,0)) mouseMovedE)
        ((\e -> if isEvent e then False else True) mouseStoppedE)
        []
    returnA -< (mouse, events)

mouseMovedEvent :: Controllable -> SF AppInput (Event (Double, Double))
mouseMovedEvent ctl =
  proc input -> do
    mouseMoved <- mouseMoving  -< input
    returnA -< mouseMoved
    
mouseStoppedEvent :: Controllable -> SF AppInput (Event ())
mouseStoppedEvent ctl =
  proc input -> do
    mouseStop <- mouseStopped -< input
    returnA -< mouseStop

-- is mouse moving or stopped?
mouseState :: Bool -> Event () -> Event () -> Bool
mouseState state moving stopped
  | isEvent moving  = True
  | isEvent stopped = False
  | otherwise       = state    
