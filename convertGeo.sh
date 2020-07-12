#!/bin/sh

# a wrapper for an otherwise lengthy expression
# @echo "converting $1 to $2 and copying to $3"

SOURCE="./resources/models/$1.geo"
TARGET="./models/$1.bgeo"
PDG="./resources/models/$1.pgeo"
PDGCPY="./models/$1.pgeo"

convertGeo(){
    if [ -e "$SOURCE" ]
    then
	python ./resources/geoParser.py $SOURCE $PDG
	cabal new-run geoIndexer $PDG $TARGET
	cp $PDG $PDGCPY
    else
	python geoParser.py # generates a ./resources/models/model.pgeo
	cabal new-run geoIndexer ./resources/models/model.pgeo ./models/model.bgeo
	cp 
    fi
}

convertGeo $1

# ./convertGeo.sh ./models/earth.geo ./models/earth.pgeo ../models/earth.bgeo
# ./convertGeo.sh earth
