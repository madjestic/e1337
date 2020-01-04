#!/bin/sh

# ## Code:
# ## mvlink linkname target
# mvlink() {
#     if [ -e "$2" ]
#     then 
# 	rm -R $1
# 	ln -s $2 $1
#     else 
# 	ln -s $2 $1
#     fi
# }

# mvlink $1 $2

# ######################################################################


# pass the path to the project as an CLI argument:
# cabal new-run e1337 ./projects/box
# cabal new-run e1337 ./projects/planet_earth
# cabal new-run e1337 ./projects/box_2mats
# cabal new-run e1337 ./projects/square
# cabal new-run e1337 ./projects/squareAndTri

# Box
# a spinning box.
mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.box
cabal new-build
cabal new-run e1337 ./projects/box

# PCloud
# a point cloud.
# mvlink ./app/Main.hs /home/madjestic/Projects/Haskell/e1337/app/Main.hs.pcloud 
# cabal new-build
# cabal new-run e1337 ./projects/box
