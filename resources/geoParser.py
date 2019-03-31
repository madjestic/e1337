# Pre parser of geo.json (arrays)

# Use example:
# $ python ./geoParser.py

import json
import sys
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

def readJSON(fileIn):
    # in:  fileIn
    # out: jsonFile
    fileIn  = "model.geo"
    fileOut = "model.pgeo" # pgeo - processed geo, in JSON format

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
    # print(indices)

    ### ATTRIBUTES ###
    attrs         = toDict (jsonDict ["attributes"])

    vtxAttrs      = attrs    ["vertexattributes"] [0]
    vtxAttrsD     = toDict (vtxAttrs [1])
    vtxAttrsDvals = vtxAttrsD ["values"]
    vtxTuples     = toDict (vtxAttrsDvals) ["tuples"]    

    ptAttrs       = attrs    ["pointattributes"] [0]
    ptAttrsD      = toDict (ptAttrs [1])
    ptAttrsDvals  = ptAttrsD ["values"] # Point Attrs Dictionary Values
    ptTuples      = toDict (ptAttrsDvals) ["tuples"]
    # print ("pTuples: ", ptTuples, "\n")

    ### FORMAT JSON ###
    data    = {}

    geoEntry = {'PGeo' : {}}
    data.update (geoEntry)

    # indices.reverse()
    jsonEntry = {'indices' : indices}
    data.get('PGeo').update(jsonEntry)
    # print (jsonEntry)

    # print(vtxTuples)
    jsonEntry = {'uv' : vtxTuples}
    data.get('PGeo').update(jsonEntry)

    jsonEntry = {'position' : ptTuples}
    # print ("pTuples: ", ptTuples, "\n")
    data.get('PGeo').update(jsonEntry)
    # print ("jsonEntry: ", jsonEntry)

    return data


def Main(fileIn = "model.geo", fileOut = "model.pgeo"):
    
    #jsonFile = readJSON(fileIn)
    data = parseJSON(readJSON(fileIn))

    # Write the data into a json fileIn
    with open(fileOut, 'w') as outfile:
        json.dump(data, outfile)

if __name__ == "__main__":
    print("This application converts Houdini geo format to a pgeo format.\n\
PGeo is monomorphic json geo container, suitable for standard haskell.\n")
    
    if len(sys.argv) <= 1:
        print("Parsing default ./model.geo")
        Main()
    else:
        print("Parsing %s" % sys.argv[1])
        Main(sys.argv[1])
