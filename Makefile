all:
	cabal new-build && gpu ./run.sh
clean :
	rm -rf ./dist-newstyle
	rm -rf ./.ghc.environment*
	rm -rf ./tags
	rm -rf ./TAGS
run:
	gpu ./run.sh

build:
	cabal new-build
