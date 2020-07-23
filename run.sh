#!/bin/sh

# Broken!
# # Spinning Objects
# # a spinning box.
# # mvlink ./app/Main.hs      /home/madjestic/Projects/Haskell/e1337/app/Main.hs.spinning_objects
# # mvlink ./src/Rendering.hs /home/madjestic/Projects/Haskell/e1337/src/Rendering.hs.earth 
# # cabal build
# # cabal run e1337 ./projects/spinning_objects

# Earth and ISS
# planet Earth modelling and shading
# mvlink ./e1337.cabal      /home/madjestic/Projects/Haskell/e1337/e1337.cabal.earth 
# mvlink ./app/Main.hs      /home/madjestic/Projects/Haskell/e1337/app/Main.hs.earth
# mvlink ./src/Rendering.hs /home/madjestic/Projects/Haskell/e1337/src/Rendering.hs.earth 
# cabal build
# cabal run e1337 ./projects/earth_iss_v02

# SDL2 IO Test
# testing SDL2 IO, such as mouse input
mvlink ./e1337.cabal      /home/madjestic/Projects/Haskell/e1337/e1337.cabal.earth 
mvlink ./app/Main.hs      /home/madjestic/Projects/Haskell/e1337/app/Main.hs.test
mvlink ./src/Rendering.hs /home/madjestic/Projects/Haskell/e1337/src/Rendering.hs.earth 
cabal build
cabal run e1337 ./projects/earth_iss_v02

# Font Rendering
# learning to render ttf with SDL/OpenGL
# mvlink ./e1337.cabal      /home/madjestic/Projects/Haskell/e1337/e1337.cabal.font 
# mvlink ./app/Main.hs      /home/madjestic/Projects/Haskell/e1337/app/Main.hs.font
# mvlink ./src/Rendering.hs /home/madjestic/Projects/Haskell/e1337/src/Rendering.hs.font
# cabal build
# cabal run e1337 ./projects/font

# Earth and ISS S3 L1
# planet Earth modelling and shading
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.earth
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.debug
# cabal build
# cabal run e1337 ./projects/iss_S3_L1

# Cornel Box
# a test scene
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.earth
# cabal build
# cabal run e1337 ./projects/cornel_box

# Indexer Debug
# a test scene
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.debug
# cabal build
# cabal run e1337 ./projects/debug

# Camera Debug
# a test scene
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.debug
# cabal build
# cabal run e1337 ./projects/debug_camera

# Broken!
# # Mandelbrot
# # playing with Mandelbrot
# # mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.mandelbrot
# # cabal build
# # cabal run e1337 ./projects/mandelbrot

