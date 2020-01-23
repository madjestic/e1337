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

# prof:
# 	make clean
# 	#ghc ./Main.hs -threaded -prof -rtsopts -fprof-auto -fprof-cafs -fforce-recomp "-with-rtsopts=-N -p -s -h -i0.1"
# 	cabalnew-build ./Main.hs -threaded -prof -rtsopts -fprof-auto -fprof-cafs -fforce-recomp "-with-rtsopts=-N -p -s -h -i0.1"
# 	./Main +RTS -sstderr -RTS

