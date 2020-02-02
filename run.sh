#!/bin/sh

# Spinning Objects
# a spinning box.
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.spinning_objects
# cabal new-build
# cabal new-run e1337 ./projects/spinning_objects

# Earth and ISS
# planet Earth modelling and shading
mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.earth
cabal new-build
cabal new-run e1337 ./projects/earth_iss
