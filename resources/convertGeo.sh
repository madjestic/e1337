#!/bin/sh

# a wrapper for an otherwise lengthy expression
# @echo "converting $1 to $2 and copying to $3"

SOURCE="./models/$1.geo"
TARGET="../models/$1.geo"
PDG="./models/$1.pgeo"
PDGCPY="../models/$1.pgeo"

convertGeo(){
    if [ -e "$SOURCE" ]
    then
	python geoParser.py $SOURCE $PDG
	./geoIndexer $PDG $TARGET
	cp $PDG $PDGCPY
    else
	python geoParser.py
	./geoIndexer ./models/model.geo ../models/model.vgeo
	cp 
    fi
}

convertGeo $1

# ./convertGeo.sh ./models/earth.geo ./models/earth.pgeo ../models/earth.vgeo
# ./convertGeo.sh earth
