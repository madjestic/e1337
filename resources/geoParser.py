# Pre parser of geo.json (arrays)

# Use example:
# $ python ./geoParser.py

import json
import sys
from itertools import chain
# from itertools import izip

def toDict (jsonFile):
    i        = iter (jsonFile)
    jsonDict = dict (zip (i, i))
    return jsonDict

def makeDict (jsonFile):
    if type(jsonFile) is list:
        return makeDict (toDict (jsonFile))
    else:
        return {jsonFile : []}

def sortArrayByIndex (array, indices):
    result = []
    for i in indices:
        result.append (array [i])
    return result

def restoreArrayFromIndex (array, indices):
    result = []
    for i in indices:
        result.append (array [i])
    return result

def readJSON(fileIn, fileOut):
    # in:  fileIn
    # out: jsonFile
    # fileIn  = "models/model.geo"
    # fileOut = "models/model.pgeo" # pgeo - processed geo, in JSON format

    outfile = open(fileIn, "r")
    jsonFile = json.loads (outfile.read ())
    outfile.close()

    return jsonFile


def parseJSON(jsonFile):
    # in:  jsonFile
    # out: jsonStructure
    
    pointcount    = (jsonFile [4])
    vertexcount   = (jsonFile [6])

    jsonDict      = toDict (jsonFile)
    ###  TOPOLOGY  ###
    topology      = toDict (jsonDict ["topology"])
    indices       = topology["pointref"][1]

    ### ATTRIBUTES ###
    attrs         = toDict (jsonDict ["attributes"])

    # DEBUG:
    # print("Alpha: ", vtxAttrs [0])
    # print("Cd: "   , vtxAttrs [1])
    # print("N: "    , vtxAttrs [2])
    # print("uv: "   , vtxAttrs [3])
    # print("P: "    , ptAttrs  [0])

    # vertex attributes list
    vtxAttrs          = attrs ["vertexattributes"]
    
    # Alpha vtx attr
    vtxAttrAlpha         = vtxAttrs [0]
    vtxAttrAlphaDict     = toDict (vtxAttrAlpha [1])
    vtxAttrAlphaDictVals = vtxAttrAlphaDict ["values"]
    vtxAttrAlphaArrays   = toDict (vtxAttrAlphaDictVals) ["arrays"]

    # Color
    vtxAttrCd         = vtxAttrs [1]
    vtxAttrCdDict     = toDict (vtxAttrCd [1])
    vtxAttrCdDictVals = vtxAttrCdDict ["values"]
    vtxAttrCdTuples   = toDict (vtxAttrCdDictVals) ["tuples"]    

    # Normal
    vtxAttrN         = vtxAttrs[2]
    vtxAttrNDict     = toDict (vtxAttrN [1])
    vtxAttrNDictVals = vtxAttrNDict ["values"]
    vtxAttrNTuples        = toDict (vtxAttrNDictVals) ["tuples"]    

    # UV
    vtxAttrUV      = vtxAttrs [3]
    vtxAttrUVDict     = toDict (vtxAttrUV [1])
    vtxAttrUVDictVals = vtxAttrUVDict ["values"]
    vtxAttrUVTuples     = toDict (vtxAttrUVDictVals) ["tuples"]    

    # Point Attributes
    ptAttrs          = attrs ["pointattributes"]
    
    # Position point attr
    ptAttrP       = ptAttrs [0]
    ptAttrPDict      = toDict (ptAttrP [1])
    ptAttrPDictVals  = ptAttrPDict ["values"] # Point Attr Dictionary Values
    ptAttrPTuples      = toDict (ptAttrPDictVals) ["tuples"]
    # print ("pTuples: ", ptAttrPTuples, "\n")

    ### FORMAT JSON ###
    data    = {}

    geoEntry = {'PGeo' : {}}
    data.update (geoEntry)

    # indices.reverse()
    jsonEntry = {'indices' : indices}
    data.get('PGeo').update(jsonEntry)
    # print (jsonEntry)

    # print(vtxTuples)
    #jsonEntry = {'Alpha' : vtxAttrAlphaArrays}
    jsonEntry = {'Alpha' : list(chain.from_iterable(vtxAttrAlphaArrays))}    
    data.get('PGeo').update(jsonEntry)
    
    jsonEntry = {'Cd' : vtxAttrCdTuples}
    data.get('PGeo').update(jsonEntry)

    jsonEntry = {'N' : vtxAttrNTuples}
    data.get('PGeo').update(jsonEntry)

    jsonEntry = {'uv' : vtxAttrUVTuples}
    data.get('PGeo').update(jsonEntry)

    jsonEntry = {'position' : ptAttrPTuples}
    # print ("pTuples: ", ptTuples, "\n")
    data.get('PGeo').update(jsonEntry)
    # print ("jsonEntry: ", jsonEntry)

    return data


def Main(fileIn = "models/model.geo", fileOut = "models/model.pgeo"):
    
    #jsonFile = readJSON(fileIn)
    data = parseJSON(readJSON(fileIn, fileOut))

    # Write the data into a json fileIn
    with open(fileOut, 'w') as outfile:
        json.dump(data, outfile)

if __name__ == "__main__":
    print("This application converts Houdini geo format to a pgeo format.\n\
PGeo is homeomorphic json geo container, suitable for standard haskell.\n\
Usage: $ python geoParser.py inputFile.geo outputFile.pgeo")
    
    if len(sys.argv) <= 1:
        print("Parsing default ./models/model.geo")
        Main()
    else:
        print("Parsing %s" % sys.argv[1])
        Main(sys.argv[1], sys.argv[2])
