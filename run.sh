#!/bin/sh

# Spinning Objects
# a spinning box.
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.spinning_objects
# cabal new-build
# cabal new-run e1337 ./projects/spinning_objects

# Earth and ISS
# planet Earth modelling and shading
mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.earth
# cabal new-build
cabal new-run e1337 ./projects/earth_iss_L3

# Test
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.earth
# cabal new-build
# cabal new-run e1337 ./projects/test

# Mandelbrot
# playing with Mandelbrot
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.mandelbrot
# cabal new-build
# cabal new-run e1337 ./projects/mandelbrot

