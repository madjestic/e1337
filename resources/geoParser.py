# Pre parser of geo.json (arrays)

# Use example:
# $ python ./geoParser.py

import json
import sys
import subprocess
from itertools import chain
from numpy import argsort,array

# from itertools import izip


def rpcShuffler(arg=[]):
    print("initializing RPC process...")
    with open('./rpcShuffler/.p2h', 'w') as f:
        f.write(str(arg))

    print("calling rpcshuffler")
    subprocess.run(["rpcShuffler/rpcshuffler"])

    with open('./rpcShuffler/.h2p', 'r') as f:
        result = f.read()

    return(result)


def concat(mlist):
    return list(chain.from_iterable(mlist))

# Fix `/` in a file path
def fixPaths(vtxAttrMatDictVals):
    result = []
    for i in vtxAttrMatDictVals:
        result.append(i[1:])
    return result


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


def readJSON(fileIn):
    # in:  fileIn
    # out: jsonFile
    # fileIn  = "models/model.geo"
    # fileOut = "models/model.pgeo" # pgeo - processed geo, in JSON format

    inFile  = open(fileIn, "r")
    #outFile = open(fileOut, "w")
    jsonFile = json.loads(inFile.read())
    inFile.close()

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

    # vertex attributes list
    vtxAttrs          = attrs ["vertexattributes"]

    ### Alpha vtx attr
    vtxAttrAlpha         = vtxAttrs [0]
    vtxAttrAlphaDict     = toDict (vtxAttrAlpha [1])
    vtxAttrAlphaDictVals = vtxAttrAlphaDict ["values"]
    #print("vtxAttrAlphaDictVals :",vtxAttrAlphaDictVals) 
    vtxAttrAlphaArrays   = toDict (vtxAttrAlphaDictVals) ["arrays"]

    ### Color
    vtxAttrCd         = vtxAttrs [1]
    vtxAttrCdDict     = toDict (vtxAttrCd [1])
    vtxAttrCdDictVals = vtxAttrCdDict ["values"]
    vtxAttrCdTuples   = toDict (vtxAttrCdDictVals) ["tuples"]    

    ### Normal
    vtxAttrN          = vtxAttrs[2]
    vtxAttrNDict      = toDict (vtxAttrN [1])
    vtxAttrNDictVals  = vtxAttrNDict ["values"]
    vtxAttrNTuples    = toDict (vtxAttrNDictVals) ["tuples"]

    # TODO : matIndices, shop_materialpath -> dir where materials live,
    # basic material = vertex shader, fragment shader, a list of textures
    ### Material
    vtxAttrMat         = vtxAttrs [3]
    vtxAttrMatDict     = toDict (vtxAttrMat [1])
    vtxAttrMatDictVals = vtxAttrMatDict ["strings"] # material paths strings
    vtxAttrMatIndices  = toDict (vtxAttrMatDict["indices"])["arrays"]


    ### UV
    vtxAttrUV         = vtxAttrs [4]
    vtxAttrUVDict     = toDict (vtxAttrUV [1])
    vtxAttrUVDictVals = vtxAttrUVDict ["values"]
    vtxAttrUVTuples   = toDict (vtxAttrUVDictVals) ["tuples"]    

    ### Point Attributes
    ptAttrs           = attrs ["pointattributes"]
    
    ### Position point attr
    ptAttrP           = ptAttrs [0]
    ptAttrPDict       = toDict (ptAttrP[1])
    ptAttrPDictVals   = ptAttrPDict ["values"] # Point Attr Dictionary Values
    ptAttrPTuples     = toDict (ptAttrPDictVals) ["tuples"]


    # DEBUG:
    # print("Alpha: ", vtxAttrs [0])
    # print("Cd: "   , vtxAttrs [1])
    # print("N: "    , vtxAttrs [2])
    # print("mat: "  , vtxAttrs [3])
    # print("uv: "   , vtxAttrs [4])
    # print("P: "    , ptAttrs  [0])
    # print("Prim: "   , ptAttrs  [0])

    ### FORMAT JSON ###
    data    = {}

    geoEntry = {'PGeo' : {}}
    data.update (geoEntry)

    # indices.reverse()
    jsonEntry = {'indices' : indices}
    #print("jsonEntry: ", jsonEntry["indices"])
    idx = jsonEntry["indices"]

    jsonEntry = {'material' : vtxAttrMatIndices}
    value = jsonEntry["material"]
    #print("initial value  :", value)
    value = concat(value)
    value = eval(rpcShuffler(value))
    # print("material indices:", vtxAttrMatIndices)
    # print("shuffled value type :", type(value))
    # print("shuffled value :", value)
    # print("shuffled value element type :", type((array(value)[0][0])))
    # print("value element :", ((array(value)[0][0])))
    # print("idx :", type(array(idx)))
    # print("idx :", idx)
    list_ = []
    for elem in (array(value)):
        list_.append((array(idx)[elem]).tolist())
        #print("list_ :", list_)
    value = list_  #value.tolist()
    #print("shuffled result :", value)
    jsonEntry = {'indices' : value}
    data.get('PGeo').update(jsonEntry)

    jsonEntry = {'Alpha' : list(chain.from_iterable(vtxAttrAlphaArrays))}    
    data.get('PGeo').update(jsonEntry)
    
    jsonEntry = {'Cd' : vtxAttrCdTuples}
    data.get('PGeo').update(jsonEntry)

    jsonEntry = {'N' : vtxAttrNTuples}
    data.get('PGeo').update(jsonEntry)

    # Fix `/` in a file path
    vtxAttrMatDictVals = fixPaths(vtxAttrMatDictVals)
    
    jsonEntry = {'material' : vtxAttrMatDictVals}
    data.get('PGeo').update(jsonEntry)

    jsonEntry = {'uv' : vtxAttrUVTuples}
    data.get('PGeo').update(jsonEntry)

    jsonEntry = {'position' : ptAttrPTuples}
    data.get('PGeo').update(jsonEntry)

    return data


def Main(fileIn = "models/model.geo", fileOut = "models/model.pgeo"):
    
    #jsonFile = readJSON(fileIn)
    data = parseJSON(readJSON(fileIn))
    # print(data)

    # Write the data into a json fileIn
    with open(fileOut, 'w') as outfile:
        json.dump(data, outfile)

if __name__ == "__main__":
    print("This application converts Houdini geo format to a pgeo, intermediary format.\n\
It's using IPC to talk to a haskell process to do the value shuffling, because I know no better :P.\n\
PGeo is homeomorphic json geo container, suitable for standard haskell.\n\
Usage: $ python geoParser.py inputFile.geo outputFile.pgeo\n")
    
    if len(sys.argv) <= 1:
        print("Parsing default ./models/model.geo")
        Main()
    else:
        print("Parsing %s" % sys.argv[1])
        Main(sys.argv[1], sys.argv[2])

# for i in list(set(concat(foo))):
# ...  list(filter(lambda x: x == i, concat(foo)))
# ... 
# [0, 0, 0]
# [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
