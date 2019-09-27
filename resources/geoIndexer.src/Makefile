all:
	make clean
	ghc -threaded -O2 ./Main.hs -o geoIndexer
	cp ./geoIndexer ../geoIndexer

prof:
	make clean
	ghc ./Main.hs -threaded -prof -rtsopts -fprof-auto -fprof-cafs -fforce-recomp "-with-rtsopts=-N -p -s -h -i0.1"
	./Main +RTS -sstderr -RTS

clean:
	rm ./Main
	rm ./Main.hi
	rm ./Main.o
