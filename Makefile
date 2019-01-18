all:
	cabal new-build && gpu ./run.sh

run:
	gpu ./run.sh

build:
	cabal new-build
