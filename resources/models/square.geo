[
	"fileversion","17.5.333",
	"hasindex",false,
	"pointcount",4,
	"vertexcount",6,
	"primitivecount",2,
	"info",{
		"date":"2019-10-21 14:38:51",
		"timetocook":0.000916999999999999955,
		"software":"Houdini 17.5.333",
		"artist":"madjestic",
		"hostname":"nu",
		"time":0,
		"bounds":[-0.5,0.5,-0.5,0.5,0,0],
		"primcount_summary":"          2 Polygons\n",
		"attribute_summary":"     5 vertex attributes:\tN, uv, shop_materialpath, Cd, Alpha\n     1 point attributes:\tP\n"
	},
	"topology",[
		"pointref",[
			"indices",[0,1,3,3,2,0]
		]
	],
	"attributes",[
		"vertexattributes",[
			[
				[
					"scope","public",
					"type","numeric",
					"name","Alpha",
					"options",{
					}
				],
				[
					"size",1,
					"storage","fpreal32",
					"defaults",[
						"size",1,
						"storage","fpreal64",
						"values",[0.998999999999999999]
					],
					"values",[
						"size",1,
						"storage","fpreal32",
						"arrays",[[0.999000013,0.999000013,0.999000013,0.999000013,0.999000013,0.999000013]
						]
					]
				]
			],
			[
				[
					"scope","public",
					"type","numeric",
					"name","Cd",
					"options",{
						"type":{
							"type":"string",
							"value":"color"
						}
					}
				],
				[
					"size",3,
					"storage","fpreal32",
					"defaults",[
						"size",1,
						"storage","fpreal64",
						"values",[1]
					],
					"values",[
						"size",3,
						"storage","fpreal32",
						"tuples",[[0.641600609,0.81477952,0.0808047056],[0.800463796,0.763874292,0.314645648],[0.510895014,0.792067647,0.708448887],[0.894846559,0.123585939,0.376660347],[0.72838676,0.0209606886,0.658269048],[0.879798412,
								0.789041281,0.531899691]
						]
					]
				]
			],
			[
				[
					"scope","public",
					"type","numeric",
					"name","N",
					"options",{
						"type":{
							"type":"string",
							"value":"normal"
						}
					}
				],
				[
					"size",3,
					"storage","fpreal32",
					"defaults",[
						"size",1,
						"storage","fpreal64",
						"values",[0]
					],
					"values",[
						"size",3,
						"storage","fpreal32",
						"tuples",[[0,0,-1],[0,0,-1],[0,0,-1],[0,0,-1],[0,0,-1],[0,0,-1]
						]
					]
				]
			],
			[
				[
					"scope","public",
					"type","string",
					"name","shop_materialpath",
					"options",{
					}
				],
				[
					"size",1,
					"storage","int32",
					"strings",["/mat/square/constant_A","/mat/square/constant_B"
					],
					"indices",[
						"size",1,
						"storage","int32",
						"arrays",[[0,0,0,1,1,1]
						]
					]
				]
			],
			[
				[
					"scope","public",
					"type","numeric",
					"name","uv",
					"options",{
						"type":{
							"type":"string",
							"value":"texturecoord"
						}
					}
				],
				[
					"size",3,
					"storage","fpreal32",
					"defaults",[
						"size",1,
						"storage","fpreal64",
						"values",[0]
					],
					"values",[
						"size",3,
						"storage","fpreal32",
						"tuples",[[0,0,0.5],[1,0,0.5],[1,1,0.5],[1,1,0.5],[0,1,0.5],[0,0,0.5]
						]
					]
				]
			]
		],
		"pointattributes",[
			[
				[
					"scope","public",
					"type","numeric",
					"name","P",
					"options",{
						"type":{
							"type":"string",
							"value":"point"
						}
					}
				],
				[
					"size",3,
					"storage","fpreal32",
					"defaults",[
						"size",1,
						"storage","fpreal64",
						"values",[0]
					],
					"values",[
						"size",3,
						"storage","fpreal32",
						"tuples",[[-0.5,-0.5,0],[0.5,-0.5,0],[-0.5,0.5,0],[0.5,0.5,0]
						]
					]
				]
			]
		]
	],
	"primitives",[
		[
			[
				"type","Polygon_run"
			],
			[
				"startvertex",0,
				"nprimitives",2,
				"nvertices",[3,3]
			]
		]
	]
]
