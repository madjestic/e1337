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

prof:
	make clean
	mvlink e1337.cabal e1337.cabal.prof
	# mvlink run.sh run.sh.prof

current:
	make clean
	mvlink e1337.cabal e1337.cabal.current
	# mvlink run.sh run.sh.current
	# ghc ./app/Main.hs -threaded -prof -rtsopts -fprof-auto -fprof-cafs -fforce-recomp "-with-rtsopts=-N -p -s -h -i0.1"
	#./Main +RTS -sstderr -RTS

