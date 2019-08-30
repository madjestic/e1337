# A space-sim game
* inspired by Strugatsky "Noon: XXII century" series.

Implemented * and planned -- features:

* Spaceship (ISS with mandlbrot fractal shading)

-- Planet.

-- Asteroids.

-- Space Trading

-- Dynamic Space Bodies

-- Realistic Gravitational Space Physics:

-- Planet Landing

-- Pirates Hunting

-- Korovan Robbing

-- Realistic Newtonian physics and Inertia

-- Ships and materials can break due to overloads, including, but not limited to, extreme changes of acceleration if applied to a mass of a material.

-- Spectral control of simulation vs arcade settings over physics and gameplay elements.

-- Relativistic Space-Time model

-- Evolving Civilisations


## Progress so far:
[youtube](https://www.youtube.com/watch?v=oiajlYck-50)

![](https://github.com/madjestic/e1337/blob/master/output.png)

## Building and running:
```bash
$ cabal new-build
$ gpu ./run.sh
```

## Rebuilding assets:
```bash
$ cd ./resources
$ make
$ cd ./resources/geoIndexer
$ ./Main
$ cp ./resources/geoIndexer/model.i.json ./src/models/iss.vgeo
```

## Working with REPL:
- compiling:
```bash
$ cabal new-repl exe:e1337
```

- running:
```bash
$ cabal new-run exe:e1337
```
- with ghcid:
```bash
$ cd ${root_dir}/app
$ ghcid "--command=ghci Main.hs -i../src"
```
- alternatively, create a file `${root_dir}/app/.ghci` with the following content:
```
:set -fwarn-unused-binds -fwarn-unused-imports
:set -i../src
:load Main
```
- then we can directly call ghci, like this:
```bash
$ cd ${root_dir}/app
$ ghcid
```

- working with nvidia-xrun
```
$ cabal new-build && nvidia-xrun run.sh
```


Bashing ideas
```
GameState:
    Position : Pos          :: V3, 64bit
             , Time         :: Integer + Float or Double
             , 3xQuaternion :: 3x4, Float
                 (acceleration for pitch, roll, yaw)
             --> 12 values + 4 = 16 -> M4

    Pos0    : Pos
            , Time
            , Orient       :: 2xQuat, 16bit
            --> 4 + 4 = 8
      // for Stellar Bodies:
            , probablity of collision -> collision map (Size -> Prob)

    Planet : Time
           , Civilization

    Civilization : Time
                 , Technology

    Technology : Technology Tree -> a point cloud, representing ideas, which have mass, so that ideas interact with gravity.  Researcers are like particles, circling around, eventually orbiting and colliding with an idea.  Orbitting an idea gives a bonus, hitting an idea gives another bonus and maybe extras, and excludes it from the PC and the sim continues.  Different ideologies or mentalities, defined as social factors, effect the geometry of the idea-space. Thus to some societies certain technologies and other objects from the idea-space take shorter or more trivial paths than to other societies, certain objects may be missing in the idea-space of certain societies and are only obtainable via interaction with other societies, this the idea-spaces are complementrary on the sense of object repertoir, the goemtry remains different. For a society that obtains originally missing idea is like finding a new branch of science, or inserting new objects into an idea-space.

, Gravity Sim to represent scientific effort
  with research units (spheres) orbit each other and collide,
  collision representing a scientific discovery or a breakthrough.
, Physical Params

, Terrain - some sort of evolution, for now can be an Int to define a state variant


Cosmic Body:
  p', Pivot     V3 (center of mass)
  p , Position  V3 + r , Orientation Q4 -> M2x4 (?)
  v , Velocity, V3
  m , Mass      F1
  d , Density,  F1


  a , Acceleration V3
  a', Angular Velocity 2xQuat (rotation speed)
```

## Output:
![](https://github.com/madjestic/e1337/blob/master/output.png)
