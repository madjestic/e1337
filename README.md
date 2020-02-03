# What is it?

At the moment it's a Shader-Toy-like application with blackjack and exotic dancers:

It supports loading and rendering simple projects files, describing a model, a material, it has a camera and supports user-input.

* Loads one or more models.
* Supports multiple materials per model.
* Applies a solver-controller per object.
* Has a camera that supports user-input.
* Renders a scene of objects.
* Uses Houdini as a game-editor.						
TODO : Support transformation stack per object.

[a youtube demo](https://youtu.be/W4Ry082HYCA)

![](https://github.com/madjestic/e1337/blob/master/output.png)

Recent Changes:

[earth_iss_scene_prototype](https://youtu.be/W4Ry082HYCA)

[multiple spinning objects](https://www.youtube.com/watch?v=Oqiyv4zhxWE)

[2 objects with different materials and centers of rotation](https://youtu.be/rnkZqXXqWYA)

[added multimaterial support](https://youtu.be/EYwBZOm5GNE)

[added multiobject support](https://youtu.be/CSedgiMpKzg)

[added multiobject update loop](https://youtu.be/S2WPuo63r5g)

# What it wants to be: A relativistic space-time-sim pilot game.
* inspired by Strugatsky "Noon: XXII century" series.

Implemented * and planned -- features:

* Spaceships (ISS with mandelbrot fractal shading).

-- Spacestations.

-- Planets.

-- Asteroids.

-- Dynamic Space Bodies with Realistic Gravitational Space Physics.

-- Planet Landing.

-- Trading.

-- Combat.

-- Exploration.

-- Pirates Hunting.

-- Korovan Robbing.

-- Realistic Newtonian physics and Inertia.

-- Ship components, crew and materials can be damaged due to overloads, including, but not limited to, extreme changes of acceleration applied a mass with certain material properties.

-- Crew-management with survivial elements.

-- Component-based destruction model.

-- Relativistic Space-Time model.

-- Dynamic Evolving Civilisations.

-- Spectral control of simulation vs arcade settings over physics and gameplay elements.

-- Hybrid Sandbox Singlplayer with Indirect Multiplayer.

-- A mix of emerging gameplay with hand-crafted scenarios and evolving tree of story-lines.

-- Direct Coop Multiplayer.

-- Community-assisted development.

## Progress so far:
[youtube](https://youtu.be/W4Ry082HYCA)

![](https://github.com/madjestic/e1337/blob/master/output.png)

## Building and running:
```bash
$ cabal new-build
$ gpu ./run.sh
```

## Rebuilding assets:
```bash
$ cd ./resources
$ ./convertGeo.sh model_file_name
```

## Working with REPL:
- compiling:
```bash
$ cabal new-repl exe:e1337
```

## Profiling
```bash
$ hp2ps -e8in -c ./e1337.hp && gv ./e1337.ps
```

- running:
```bash
$ gpu ./run.sh
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
