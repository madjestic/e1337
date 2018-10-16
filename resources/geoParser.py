# Pre parser of geo.json (arrays)
import json
from itertools import izip

def toDict (jsonFile):
    i        = iter (jsonFile)
    jsonDict = dict (izip (i, i))
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

fileIn  = "model.geo"
fileOut = "model.pgeo" # pgeo - processed geo, in JSON format

outfile = file(fileIn, "r")
jsonFile = json.loads (outfile.read ())
outfile.close()

pointcount    = (jsonFile [4])
vertexcount   = (jsonFile [6])

jsonDict      = toDict (jsonFile)
###  TOPOLOGY  ###
topology      = toDict (jsonDict ["topology"])
indices       = topology["pointref"][1]

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

### FORMAT JSON ###
data    = {}

geoEntry = {'PGeo' : {}}
data.update (geoEntry)

# indices.reverse()
jsonEntry = {'indices' : indices}
data.get('PGeo').update(jsonEntry)

jsonEntry = {'uv' : vtxTuples}
data.get('PGeo').update(jsonEntry)

ptTuples  = restoreArrayFromIndex (ptTuples, indices)
jsonEntry = {'position' : ptTuples}
data.get('PGeo').update(jsonEntry)

# Write the data into a json file
with open(fileOut, 'w') as outfile:
    json.dump(data, outfile)

### FORMAT Geometry.hs ###
dataStr = ""
space    = "          "

dataStr += "module Geometry where       \n" # refactor as module Geometry where
dataStr += "                            \n"
dataStr += "import Types                \n"
dataStr += "                            \n"
dataStr += "model :: PGeo               \n"
dataStr += "model =                     \n"
dataStr += "  PGeo ps uvs               \n"
dataStr += "  where                     \n"
dataStr += "    uvs = [                 \n"
for vTuple in vtxTuples:
    dataStr += space + str ( tuple (vTuple)) +  ",\n"
dataStr  = dataStr[:-2] + "\n"              # remove the trailing ','
dataStr += "          ]                 \n"
dataStr += "    ps  = [                 \n"
for ptTuple in ptTuples:
    dataStr += space + str ( tuple (ptTuple)) + ",\n"
dataStr  = dataStr[:-2] + "\n"              # remove the trailing ','
dataStr += "          ]                 \n"    

# dataStr += str (ptTuples)

strOut = "Geometry.hs"    
with open(strOut, 'w') as outfile:
    outfile.write (dataStr)
outfile.close()
