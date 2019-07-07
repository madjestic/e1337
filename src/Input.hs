{-# LANGUAGE MultiWayIf #-}
module Input
    ( AppInput
    , parseWinInput
    , mousePos
    , lbp
    , lbpPos
    , lbDown
    , rbp
    , rbpPos
    , rbDown
    , key
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

-- | Events that indicate left button click
lbp :: SF AppInput (Event ())
lbp = lbpPos >>^ tagWith ()

-- | Events that indicate left button click and are tagged with mouse position
lbpPos :: SF AppInput (Event (Double,Double))
lbpPos = inpMouseLeft ^>> edgeJust

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

key :: SDL.Scancode -> String -> SF AppInput (Event ())
key code mode =
  (inpKeyMode ^>> edgeJust) >>^ filterE (code ==) >>^ tagWith ()
  where
    inpKeyMode
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

quitEvent :: SF AppInput (Event ())
quitEvent = arr inpQuit >>> edge

data AppInput =
     AppInput
     { inpMousePos          :: (Double, Double)       -- ^ Current mouse position
     , inpMouseLeft         :: Maybe (Double, Double) -- ^ Left   button currently down
     , inpMouseRight        :: Maybe (Double, Double) -- ^ Right  button currently down
     --, inpMouseMiddle       :: Maybe (Double, Double) -- ^ Middle button currently down
     , inpQuit              :: Bool                   -- ^ SDL's QuitEvent
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
     }

type WinInput = Event SDL.EventPayload    

initAppInput :: AppInput
initAppInput =
     AppInput 
     { inpMousePos          = (0, 0)
     , inpMouseLeft         = Nothing
     , inpMouseRight        = Nothing
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
    inp { inpMousePos = (fromIntegral x, fromIntegral y) }
    where P (V2 x y) = SDL.mouseMotionEventPos ev
-- | key events
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
                
-- | mouse button events              
nextAppInput inp (SDL.MouseButtonEvent ev) = inp { inpMouseLeft  = lmb
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

nextAppInput inp _ = inp
