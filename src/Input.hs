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
    , key'
    , quitEvent
    , module SDL.Input.Keyboard.Codes
    ) where

import           Data.Maybe

import           FRP.Yampa

import           Linear (V2(..))
import           Linear.Affine (Point(..))

import           SDL.Input.Keyboard.Codes
import qualified SDL

import           Keys

-- import Debug.Trace as DT

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
key code mode 
  | code `elem`
    [ SDL.ScancodeW     
    , SDL.ScancodeS     
    , SDL.ScancodeA     
    , SDL.ScancodeD     
    , SDL.ScancodeQ     
    , SDL.ScancodeE     
    , SDL.ScancodeX     
    , SDL.ScancodeZ     
    , SDL.ScancodeUp    
    , SDL.ScancodeDown  
    , SDL.ScancodeLeft  
    , SDL.ScancodeRight 
    , SDL.ScancodeSpace ]
  = (inpKeyMode ^>> edgeJust) >>^ filterE (code ==) >>^ tagWith ()
  where
    inpKeyMode
      = if | mode == "Pressed" -> inpKeySpacePressed
           | otherwise         -> inpKeySpaceReleased

key' :: SDL.Scancode -> String -> SF AppInput (Event ())
key' code mode =
  (inpKeyMode ^>> edgeJust) >>^ filterE (code ==) >>^ tagWith ()
  where
    inpKeyMode
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
-- TODO : finish ^

quitEvent :: SF AppInput (Event ())
quitEvent = arr inpQuit >>> edge

-- | Exported as abstract type. Fields are accessed with signal functions.
-- | AppInput ~= AppInput
data AppInput =
     AppInput
     { inpMousePos     :: (Double, Double)       -- ^ Current mouse position
     , inpMouseLeft    :: Maybe (Double, Double) -- ^ Down button currently down
     , inpMouseRight   :: Maybe (Double, Double) -- ^ Right button currently down
     , inpQuit         :: Bool                   -- ^ SDL's QuitEvent
     , inpKeySpacePressed   :: Maybe SDL.Scancode
     , inpKeySpaceReleased  :: Maybe SDL.Scancode
     , inpKeyWPressed  :: Maybe SDL.Scancode
     , inpKeyWReleased :: Maybe SDL.Scancode
     , inpKeySPressed  :: Maybe SDL.Scancode
     , inpKeySReleased :: Maybe SDL.Scancode
     , inpKeyAPressed  :: Maybe SDL.Scancode
     , inpKeyAReleased :: Maybe SDL.Scancode
     , inpKeyDPressed  :: Maybe SDL.Scancode
     , inpKeyDReleased :: Maybe SDL.Scancode
     }

type WinInput = Event SDL.EventPayload    

initAppInput :: AppInput
initAppInput =
     AppInput
     { inpMousePos     = (0, 0)
     , inpMouseLeft    = Nothing
     , inpMouseRight   = Nothing
     , inpQuit         = False
     , inpKeySpacePressed   = Nothing
     , inpKeySpaceReleased  = Nothing
     , inpKeyWPressed  = Nothing
     , inpKeyWReleased = Nothing
     , inpKeySPressed  = Nothing
     , inpKeySReleased = Nothing
     , inpKeyAPressed  = Nothing
     , inpKeyAReleased = Nothing
     , inpKeyDPressed  = Nothing
     , inpKeyDReleased = Nothing
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
                
-- | mouse button events              
nextAppInput inp (SDL.MouseButtonEvent ev) = inp { inpMouseLeft  = lmb
                                                 , inpMouseRight = rmb }
    where motion = SDL.mouseButtonEventMotion ev
          button = SDL.mouseButtonEventButton ev
          pos    = inpMousePos inp
          inpMod = case (motion,button) of
              (SDL.Released, SDL.ButtonLeft)  -> first  (const Nothing)
              (SDL.Pressed,  SDL.ButtonLeft)  -> first  (const (Just pos))
              (SDL.Released, SDL.ButtonRight) -> second (const Nothing)
              (SDL.Pressed,  SDL.ButtonRight) -> second (const (Just pos))
              _                               -> id
          (lmb,rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) inp

nextAppInput inp _ = inp
