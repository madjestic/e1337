all:
	make clean
	ghc -threaded -O2 ./Main.hs -o geoIndexer
	mv ./geoIndexer ../geoIndexer

fast:
	# make clean
	ghc -threaded -O2 ./Main.hs -o geoIndexer
	mv ./geoIndexer ../geoIndexer

prof:
	make clean
	ghc ./Main.hs -threaded -prof -rtsopts -fprof-auto -fprof-cafs -fforce-recomp "-with-rtsopts=-N -p -s -h -i0.1"
	./Main +RTS -sstderr -RTS

clean:
	rm ../geoIndexer
	# rm ./Main.hi
	# rm ./Main.o
