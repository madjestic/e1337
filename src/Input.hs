{-# LANGUAGE MultiWayIf #-}
module Input
    ( AppInput
    , parseWinInput
    , mousePos
    , mouseMotion
    , mouseRelPos
    , mouseEvent
    , lbp
    , lbpPos
    , lbDown
    , rbp
    , rbpPos
    , rbDown
    , keyInput
    , quitEvent
    , module SDL.Input.Keyboard.Codes
    ) where

import           Data.Maybe

import           FRP.Yampa

import           Linear (V2(..))
import           Linear.Affine (Point(..))

import           SDL.Input.Keyboard.Codes
import qualified SDL

import           Keyboard

import Debug.Trace as DT

-- <| Signal Functions |> --
-- | Current mouse position
mousePos :: SF AppInput (Double,Double)
mousePos = arr inpMousePos

mouseMotion :: SF AppInput (Event ())
mouseMotion = mouseRelPos >>^ tagWith ()

mouseRelPos :: SF AppInput (Event (Double,Double))
mouseRelPos = inpMouseRelPos ^>> edgeJust

-- | Events that indicate left button click
lbp :: SF AppInput (Event ())
lbp = lbpPos >>^ tagWith ()

-- | Events that indicate left button click and are tagged with mouse position
lbpPos :: SF AppInput (Event (Double,Double))
lbpPos = inpMouseLeft ^>> edgeJust

-- mme :: SF AppInput (Event ())
-- mme = mmePos >>^ tagWith ()

-- mmePos :: SF AppInput (Event (Double, Double))
-- mmePos = inpMousePos ^>> edgeJust

-- | Is left button down
lbDown :: SF AppInput Bool
lbDown = arr (isJust . inpMouseLeft)

-- | Events that indicate right button click
rbp :: SF AppInput (Event ())
rbp = rbpPos >>^ tagWith ()

-- | Events that indicate right button click and are tagged with mouse position
rbpPos :: SF AppInput (Event (Double,Double))
rbpPos = inpMouseRight ^>> edgeJust

-- | Is right button down
rbDown :: SF AppInput Bool
rbDown = arr (isJust . inpMouseRight)

mouseEvent :: SF AppInput (Event ())
mouseEvent = arr inpMouseMoving >>> edge

quitEvent :: SF AppInput (Event ())
quitEvent = arr inpQuit >>> edge

keyInput :: SDL.Scancode -> String -> SF AppInput (Event ())
keyInput code mode =
  (inpKeyMode ^>> edgeJust) >>^ filterE (code ==) >>^ tagWith ()
  where
    inpKeyMode
      -- | code == code = inpMousePos -- TODO: attempting to add mouse input sensing
      | code == SDL.ScancodeSpace
      = if | mode == "Pressed" -> inpKeySpacePressed
           | otherwise         -> inpKeySpaceReleased           
      | code == SDL.ScancodeW
      = if | mode == "Pressed" -> inpKeyWPressed
           | otherwise         -> inpKeyWReleased
      | code == SDL.ScancodeS
      = if | mode == "Pressed" -> inpKeySPressed
           | otherwise         -> inpKeySReleased
      | code == SDL.ScancodeA
      = if | mode == "Pressed" -> inpKeyAPressed
           | otherwise         -> inpKeyAReleased
      | code == SDL.ScancodeD
      = if | mode == "Pressed" -> inpKeyDPressed
           | otherwise         -> inpKeyDReleased
      | code == SDL.ScancodeQ
      = if | mode == "Pressed" -> inpKeyQPressed
           | otherwise         -> inpKeyQReleased
      | code == SDL.ScancodeE
      = if | mode == "Pressed" -> inpKeyEPressed
           | otherwise         -> inpKeyEReleased
      | code == SDL.ScancodeZ
      = if | mode == "Pressed" -> inpKeyZPressed
           | otherwise         -> inpKeyZReleased
      | code == SDL.ScancodeX
      = if | mode == "Pressed" -> inpKeyXPressed
           | otherwise         -> inpKeyXReleased
      | code == SDL.ScancodeC
      = if | mode == "Pressed" -> inpKeyCPressed
           | otherwise         -> inpKeyCReleased           
      | code == SDL.ScancodeUp
      = if | mode == "Pressed" -> inpKeyUpPressed
           | otherwise         -> inpKeyUpReleased
      | code == SDL.ScancodeDown
      = if | mode == "Pressed" -> inpKeyDownPressed
           | otherwise         -> inpKeyDownReleased
      | code == SDL.ScancodeLeft
      = if | mode == "Pressed" -> inpKeyLeftPressed
           | otherwise         -> inpKeyLeftReleased           
      | code == SDL.ScancodeRight
      = if | mode == "Pressed" -> inpKeyRightPressed
           | otherwise         -> inpKeyRightReleased
      | code == SDL.ScancodePageUp
      = if | mode == "Pressed" -> inpKeyPageUpPressed
           | otherwise         -> inpKeyPageUpReleased           
      | code == SDL.ScancodePageDown
      = if | mode == "Pressed" -> inpKeyPageDownPressed
           | otherwise         -> inpKeyPageDownReleased
      | code == SDL.ScancodeLShift
      = if | mode == "Pressed" -> inpKeyLShiftPressed
           | otherwise         -> inpKeyLShiftReleased           
      | code == SDL.ScancodeLCtrl
      = if | mode == "Pressed" -> inpKeyLCtrlPressed
           | otherwise         -> inpKeyLCtrlReleased
      | code == SDL.ScancodeRShift
      = if | mode == "Pressed" -> inpKeyRShiftPressed
           | otherwise         -> inpKeyRShiftReleased           
      | code == SDL.ScancodeRCtrl
      = if | mode == "Pressed" -> inpKeyRCtrlPressed
           | otherwise         -> inpKeyRCtrlReleased

--mouseInput ::            

data AppInput =
     AppInput
     { inpMousePos          :: (Double, Double)       -- ^ Current mouse position
     , inpMouseRelPos       :: Maybe (Double, Double) -- ^ Relative mouse motion
     , inpMouseLeft         :: Maybe (Double, Double) -- ^ Left   button currently down
     , inpMouseRight        :: Maybe (Double, Double) -- ^ Right  button currently down
     --, inpMouseMiddle       :: Maybe (Double, Double) -- ^ Middle button currently down
     , inpQuit              :: Bool                   -- ^ SDL's QuitEvent
     , inpMouseMoving       :: Bool
     , inpMouseStopped      :: Bool
     , inpKeySpacePressed   :: Maybe SDL.Scancode
     , inpKeySpaceReleased  :: Maybe SDL.Scancode
     -- W
     , inpKeyWPressed       :: Maybe SDL.Scancode
     , inpKeyWReleased      :: Maybe SDL.Scancode
     -- S                   
     , inpKeySPressed       :: Maybe SDL.Scancode
     , inpKeySReleased      :: Maybe SDL.Scancode
     -- A                   
     , inpKeyAPressed       :: Maybe SDL.Scancode
     , inpKeyAReleased      :: Maybe SDL.Scancode
     -- D                   
     , inpKeyDPressed       :: Maybe SDL.Scancode
     , inpKeyDReleased      :: Maybe SDL.Scancode
     -- Q                   
     , inpKeyQPressed       :: Maybe SDL.Scancode
     , inpKeyQReleased      :: Maybe SDL.Scancode
     -- E                   
     , inpKeyEPressed       :: Maybe SDL.Scancode
     , inpKeyEReleased      :: Maybe SDL.Scancode
     -- Z                   
     , inpKeyZPressed       :: Maybe SDL.Scancode
     , inpKeyZReleased      :: Maybe SDL.Scancode
     -- X                   
     , inpKeyXPressed       :: Maybe SDL.Scancode
     , inpKeyXReleased      :: Maybe SDL.Scancode
     -- C                   
     , inpKeyCPressed       :: Maybe SDL.Scancode
     , inpKeyCReleased      :: Maybe SDL.Scancode
     -- Up
     , inpKeyUpPressed      :: Maybe SDL.Scancode
     , inpKeyUpReleased     :: Maybe SDL.Scancode
     -- Down
     , inpKeyDownPressed    :: Maybe SDL.Scancode
     , inpKeyDownReleased   :: Maybe SDL.Scancode
     -- Left
     , inpKeyLeftPressed    :: Maybe SDL.Scancode
     , inpKeyLeftReleased   :: Maybe SDL.Scancode
     -- Right
     , inpKeyRightPressed   :: Maybe SDL.Scancode
     , inpKeyRightReleased  :: Maybe SDL.Scancode
     -- PageUp
     , inpKeyPageUpPressed   :: Maybe SDL.Scancode
     , inpKeyPageUpReleased  :: Maybe SDL.Scancode
     -- PageDown
     , inpKeyPageDownPressed   :: Maybe SDL.Scancode
     , inpKeyPageDownReleased  :: Maybe SDL.Scancode
     -- LShift
     , inpKeyLShiftPressed   :: Maybe SDL.Scancode
     , inpKeyLShiftReleased  :: Maybe SDL.Scancode     
     -- LCtrl
     , inpKeyLCtrlPressed   :: Maybe SDL.Scancode
     , inpKeyLCtrlReleased  :: Maybe SDL.Scancode
     -- RShift
     , inpKeyRShiftPressed   :: Maybe SDL.Scancode
     , inpKeyRShiftReleased  :: Maybe SDL.Scancode     
     -- RCtrl
     , inpKeyRCtrlPressed   :: Maybe SDL.Scancode
     , inpKeyRCtrlReleased  :: Maybe SDL.Scancode     
     }

type WinInput = Event SDL.EventPayload    

initAppInput :: AppInput
initAppInput =
     AppInput 
     { inpMousePos          = (0.0, 0.0)
     , inpMouseRelPos       = Just (0.0, 0.0)
     , inpMouseLeft         = Nothing
     , inpMouseRight        = Nothing
     , inpMouseMoving       = False
     , inpMouseStopped      = True
     --, inpMouseMiddle       = Nothing
     , inpQuit              = False
     , inpKeySpacePressed   = Nothing
     , inpKeySpaceReleased  = Nothing
     -- W
     , inpKeyWPressed       = Nothing
     , inpKeyWReleased      = Nothing
     -- S                   
     , inpKeySPressed       = Nothing
     , inpKeySReleased      = Nothing
     -- A                   
     , inpKeyAPressed       = Nothing
     , inpKeyAReleased      = Nothing
     -- D                   
     , inpKeyDPressed       = Nothing
     , inpKeyDReleased      = Nothing
     -- Q                   
     , inpKeyQPressed       = Nothing
     , inpKeyQReleased      = Nothing
     -- E                   
     , inpKeyEPressed       = Nothing
     , inpKeyEReleased      = Nothing
     -- Z                   
     , inpKeyZPressed       = Nothing
     , inpKeyZReleased      = Nothing
     -- X                   
     , inpKeyXPressed       = Nothing
     , inpKeyXReleased      = Nothing
     -- C                   
     , inpKeyCPressed       = Nothing
     , inpKeyCReleased      = Nothing     
     -- Up
     , inpKeyUpPressed      = Nothing
     , inpKeyUpReleased     = Nothing
     -- Down
     , inpKeyDownPressed    = Nothing
     , inpKeyDownReleased   = Nothing
     -- Left
     , inpKeyLeftPressed    = Nothing
     , inpKeyLeftReleased   = Nothing
     -- Right
     , inpKeyRightPressed   = Nothing
     , inpKeyRightReleased  = Nothing
     -- PageUp
     , inpKeyPageUpPressed   = Nothing
     , inpKeyPageUpReleased  = Nothing
     -- PageDown
     , inpKeyPageDownPressed   = Nothing
     , inpKeyPageDownReleased  = Nothing
     -- LShift
     , inpKeyLShiftPressed   = Nothing
     , inpKeyLShiftReleased  = Nothing     
     -- LCtrl
     , inpKeyLCtrlPressed   = Nothing
     , inpKeyLCtrlReleased  = Nothing
     -- RShift
     , inpKeyRShiftPressed   = Nothing
     , inpKeyRShiftReleased  = Nothing     
     -- RCtrl
     , inpKeyRCtrlPressed   = Nothing
     , inpKeyRCtrlReleased  = Nothing          
     }

-- | Filter and transform SDL events into events which are relevant to our
--   application
parseWinInput :: SF WinInput AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

scancode :: SDL.KeyboardEventData -> Scancode
scancode ev =
  SDL.keysymScancode $ SDL.keyboardEventKeysym ev

nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
-- | quit event
nextAppInput inp SDL.QuitEvent
  = inp { inpQuit = True }
-- | mouse movement/position
nextAppInput inp (SDL.MouseMotionEvent ev) =
    inp { inpMouseMoving  = True
        , inpMouseStopped = False
        , inpMousePos     = (fromIntegral x, fromIntegral y)
        , inpMouseRelPos  = Just (fromIntegral x', fromIntegral y') }
    where (P (V2 x  y ))  = SDL.mouseMotionEventPos ev
          (  (V2 x' y'))  = SDL.mouseMotionEventRelMotion ev
-- | keyInput events
nextAppInput inp (SDL.KeyboardEvent ev)
    | scancode ev == SDL.ScancodeEscape
      = inp { inpQuit = True }
      
    | (scancode ev) == SDL.ScancodeSpace
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeySpacePressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeySpaceReleased = Nothing }
           | otherwise
             -> inp { inpKeySpacePressed  = Nothing
                    , inpKeySpaceReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeW
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyWPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyWReleased = Nothing }
           | otherwise       
             -> inp { inpKeyWPressed  = Nothing
                    , inpKeyWReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeS
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeySPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeySReleased = Nothing }
           | otherwise
             -> inp { inpKeySPressed  = Nothing
                    , inpKeySReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
                
    | (scancode ev) == SDL.ScancodeA
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyAPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyAReleased = Nothing }
           | otherwise
             -> inp { inpKeyAPressed  = Nothing
                    , inpKeyAReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeD
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyDPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyDReleased = Nothing }
           | otherwise
             -> inp { inpKeyDPressed  = Nothing
                    , inpKeyDReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeQ
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyQPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyQReleased = Nothing }
           | otherwise
             -> inp { inpKeyQPressed  = Nothing
                    , inpKeyQReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeE
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyEPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyEReleased = Nothing }
           | otherwise      
             -> inp { inpKeyEPressed  = Nothing
                    , inpKeyEReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeZ
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyZPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyZReleased = Nothing }
           | otherwise      
             -> inp { inpKeyZPressed  = Nothing
                    , inpKeyZReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
                
    | (scancode ev) == SDL.ScancodeX
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyXPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyXReleased = Nothing }
           | otherwise      
             -> inp { inpKeyXPressed  = Nothing
                    , inpKeyXReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeC
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyCPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyCReleased = Nothing }
           | otherwise      
             -> inp { inpKeyCPressed  = Nothing
                    , inpKeyCReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeLShift
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyLShiftPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyLShiftReleased = Nothing }
           | otherwise      
             -> inp { inpKeyLShiftPressed  = Nothing
                    , inpKeyLShiftReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
                
    | (scancode ev) == SDL.ScancodeLCtrl
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyLCtrlPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyLCtrlReleased = Nothing }
           | otherwise      
             -> inp { inpKeyLCtrlPressed  = Nothing
                    , inpKeyLCtrlReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeRShift
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyRShiftPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyRShiftReleased = Nothing }
           | otherwise      
             -> inp { inpKeyRShiftPressed  = Nothing
                    , inpKeyRShiftReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
                
    | (scancode ev) == SDL.ScancodeRCtrl
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyRCtrlPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyRCtrlReleased = Nothing }
           | otherwise      
             -> inp { inpKeyRCtrlPressed  = Nothing
                    , inpKeyRCtrlReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodePageUp
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyPageUpPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyPageUpReleased = Nothing }
           | otherwise      
             -> inp { inpKeyPageUpPressed  = Nothing
                    , inpKeyPageUpReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodePageDown
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyPageDownPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyPageDownReleased = Nothing }
           | otherwise      
             -> inp { inpKeyPageDownPressed  = Nothing
                    , inpKeyPageDownReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeUp
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyUpPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyUpReleased = Nothing }
           | otherwise      
             -> inp { inpKeyUpPressed  = Nothing
                    , inpKeyUpReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeDown
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyDownPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyDownReleased = Nothing }
           | otherwise      
             -> inp { inpKeyDownPressed  = Nothing
                    , inpKeyDownReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeLeft
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyLeftPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyLeftReleased = Nothing }
           | otherwise      
             -> inp { inpKeyLeftPressed  = Nothing
                    , inpKeyLeftReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeRight
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyRightPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyRightReleased = Nothing }
           | otherwise      
             -> inp { inpKeyRightPressed  = Nothing
                    , inpKeyRightReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }

    | (scancode ev) == SDL.ScancodeRShift
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyRShiftPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyRShiftReleased = Nothing }
           | otherwise      
             -> inp { inpKeyRShiftPressed  = Nothing
                    , inpKeyRShiftReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
                
    | (scancode ev) == SDL.ScancodeRCtrl
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
             -> inp { inpKeyRCtrlPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                    , inpKeyRCtrlReleased = Nothing }
           | otherwise      
             -> inp { inpKeyRCtrlPressed  = Nothing
                    , inpKeyRCtrlReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
                
-- | mouse button events              
nextAppInput inp (SDL.MouseButtonEvent ev)
  = inp { inpMouseLeft  = lmb
        , inpMouseRight = rmb }
    where motion = SDL.mouseButtonEventMotion ev
          button = SDL.mouseButtonEventButton ev
          pos    = inpMousePos inp
          (lmb,rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) inp
            where
              inpMod = case (motion,button) of
                (SDL.Released, SDL.ButtonLeft)   -> first  (const Nothing)
                (SDL.Pressed,  SDL.ButtonLeft)   -> first  (const (Just pos))
                (SDL.Released, SDL.ButtonRight)  -> second (const Nothing)
                (SDL.Pressed,  SDL.ButtonRight)  -> second (const (Just pos))
                -- (SDL.Released, SDL.ButtonMiddle) -> second (const Nothing)
                -- (SDL.Pressed,  SDL.ButtonMiddle) -> second (const (Just pos))
                _ -> id
nextAppInput inp _ =
  inp { inpMouseMoving  = False
      , inpMouseStopped = True
      , inpMouseRelPos  = Nothing
      }

-- nextAppInput inp _ = inp
