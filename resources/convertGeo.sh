#!/bin/sh

# a wrapper for an otherwise lengthy expression
# @echo "converting $1 to $2 and copying to $3"

convertGeo(){
    if [ -e "$1" ]
    then
	python geoParser.py $1 $2
	./geoIndexer $2 $3
    else
	python geoParser.py
	./geoIndexer ./models/model.geo ../models/model.vgeo
    fi
}

convertGeo $1 $2 $3

# ./convertGeo.sh ./models/earth.geo ./models/earth.pgeo ../models/earth.vgeo
