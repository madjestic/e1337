{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Object
  ( Object (..)
  , defaultObj
  , materials
  , programs
  , descriptors
  , transforms
  , solve
  , solver
  , solvers
  , loadObjects
  , initObject
  , updateObjects
  ) where

import Linear.V4
import Linear.Matrix -- (M44, M33, identity, translation, fromQuaternion, (!*!), mkTransformationMat)
import Linear (V3(..))
import Control.Lens hiding (transform)
import FRP.Yampa    hiding (identity)
import Graphics.Rendering.OpenGL (Program (..), ShaderType (..))

import LoadShaders
import Material
import Descriptor
import Solvable
import PGeo
import VGeo
import Project
import Model
import Utils

import Debug.Trace    as DT
  
--------------------------------------------------------------------------------
-- < Object > ------------------------------------------------------------------

data Object
  =  Object
     { 
       _descriptors :: [Descriptor] -- | Material is bound in Descriptor, but we also use this data for draw-call separation per material.
                -- data Descriptor =
                     -- Descriptor VertexArrayObject NumArrayIndices
     , _materials   :: [Material]   -- | hence [Material] is present on the Object level too, we use that value, instead of looking it up from respective VGeo.
     , _programs    :: [Program]    -- | Shader Programs
     , _transforms  :: [M44 Double]
     , _velocity    :: V3 Double
     , _avelocity   :: V3 Double    -- | Angular velocity
     , _mass        :: Double
     , _density     :: Double
     , _time        :: Double
     , _solvers     :: [Solver]
     } deriving Show

$(makeLenses ''Object)


-- TODO : take translation (pivot offset) into account
instance Solvable Object where
  solver :: Solver -> Object -> SF () (Object)
  solver (Rotate pv0 ypr0) obj0 = -- TODO: there are multiple transforms per object
    proc () -> do
      --ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr0
      ypr' <- ((V3 0 0 0) ^+^) ^<< integral -< ypr0
      _ <- DT.trace ("Object._transforms obj0 :" ++ show (Object._transforms obj0)) $ returnA -< ()
      let mtx0 = Object._transforms obj0
          mtx = undefined :: [M44 Double]
            -- mkTransformationMat
            -- rot
            -- tr
            -- where
            --   rot =
            --     (view _m33 mtx0)
            --     !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
            --     !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
            --     !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
            --   --tr  = DT.trace ("view translation mtx0: " ++ show (view (_w._xyz) mtx0)) $ view (_w._xyz) mtx0
            --   --tr  = V3 0.777 0 0
            --  tr  = view (_w._xyz) mtx0
      --returnA -< obj0 { Object._transforms = (DT.trace ("mtx :" ++ show mtx) $ mtx) }
      returnA -< obj0 { Object._transforms = mtx }

-- spin :: V3 Double -> V3 Double -> M44 Double -> M44 Double
-- spin pivot rot mtx = undefined

defaultObj :: Object
defaultObj =
  Object.Object
    []
    [defaultMat]
    []
    [(identity::M44 Double)]
    (V3 0 0 0)
    (V3 0 0 0)
    (1.0)
    (1.0)
    (0.0)
    []

loadObjects :: (([Int], Int, [Float], Material) -> IO Descriptor) -> Project -> IO [Object]
loadObjects initVAO project = 
  do
    -- _ <- DT.trace ("project :" ++ show project) $ return ()
    print "Loading Models..."
    objVGeos <- mapM (\modelPath ->
                      do { vgeo <- readBGeo modelPath :: IO VGeo
                         ; return vgeo
                         }
                  ) $ toListOf (models . traverse . Model.path) project :: IO [VGeo]
    objs <- mapM (initObject initVAO) objVGeos :: IO [Object] -- object per vgeo
    print "Finished loading models."
    
    return (objs)

initObject :: (([Int], Int, [Float], Material) -> IO Descriptor) -> VGeo -> IO Object
initObject initVAO vgeo =
  do
    mats  <- mapM readMaterial $ ms vgeo :: IO [Material]
    let (VGeo is_ st_ vs_ ms_ xf_) = vgeo
        vaoArgs = (\idx' st' vao' mat' ->  (idx', st', vao', mat')) <$.> is_ <*.> st_ <*.> vs_ <*.> mats
        offset = fmap ((view _w).fromList) (xf_)
        preTransforms = fmap fromList ((xf_))

    ds <- mapM initVAO vaoArgs

    progs <- mapM (\mat -> loadShaders
                           [ ShaderInfo VertexShader   (FileSource (_vertShader (mat) ))
                           , ShaderInfo FragmentShader (FileSource (_fragShader (mat) )) ]) mats

    let obj =
          defaultObj
          { _descriptors = ds
          , _materials   = mats
          , _programs    = progs
          , _transforms  = preTransforms
          -- , _pivot       = view _xyz offset
          -- , _solvers     =
          --   [(Rotate (view _xyz offset) (V3 0 0 1000))] -- TODO: a solver per ()transform) object
          , _solvers = [] -- fmap (\offset' -> (Rotate (view _xyz offset') (V3 0 0 1000))) offset
          } :: Object

    return obj

fromVGeo :: (([Int], Int, [Float], Material) -> IO Descriptor) -> VGeo -> IO Object
fromVGeo initVAO (VGeo idxs st vaos matPaths xform) = 
  do
    mats <- mapM readMaterial matPaths -- (ms vgeo)
    let
      vaoargs         = (\idx' st' vao' mat' ->  (idx', st', vao', mat')) <$.> idxs <*.> st <*.> vaos <*.> mats
      offset = view _w (fromList (xform!!0) :: M44 Double) -- xform!!0 - at conversion stage, there can be only a single element in a list of transforms, therefore I don't want to overcomplicate it at the moment and keep it adhoc.

    ds   <- mapM initVAO vaoargs
    
    let object =
          defaultObj
          { _descriptors = ds
          , _materials   = mats
          --, _transforms   = preTransform
          --, _pivot       = offset
          , _solvers     =
            [(Rotate (view _xyz offset) (V3 0 0 1000))]
          }

    return object
    
-- TODO : Object -> Solver -> SF () Object
solve :: Object -> SF () Object
solve obj =
  proc () -> do
-- TODO: read pivot from the object preTransforms
    mtxs <- (parB . fmap (\mtx -> spin (V3 0 0 0) (V3 0 (0) (0*1000)) mtx)) (_transforms obj) -< ()
    --obj' <- mapM (\pv0' -> solver (Rotate (pv0') (V3 0 0 1000)) obj) pv0 -< ()
    returnA -< obj { _transforms = mtxs }
    --returnA -< obj-- { _transforms = mtxs }
      -- where
      --   pv0 = toListOf (Obj.transform . traverse . _w . _xyz) obj

-- TODO: [Object] -> [Solver] -> SF () [Object]
updateObjects :: [Object] -> SF () [Object]
updateObjects = parB . fmap solve
