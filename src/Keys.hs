module Keys
  ( Keys (..)
  ) where

data Keys =
     Keys
     { keyW     :: Bool
     , keyS     :: Bool
     , keyA     :: Bool
     , keyD     :: Bool
     , keyQ     :: Bool
     , keyE     :: Bool
     , keyZ     :: Bool
     , keyX     :: Bool
     , keyUp    :: Bool
     , keyDown  :: Bool
     , keyLeft  :: Bool
     , keyRight :: Bool
     } deriving Show
