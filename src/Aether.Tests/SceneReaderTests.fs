﻿namespace ``SceneReader``

open Xunit
open FsUnit.Xunit
open Aether


type ``Given a full scene file`` () =
  let input = """
      # From http://www.randelshofer.ch/jpbrt/scenes/02-kon-tiki.pbrt
      #
      # @(#)02-kon-tiki.pbrt  1.2.2  2010-09-22
      # 
      Film "image" 
	      "integer xresolution" [400] "integer yresolution" [400]
	      "string filename" "02-kon-tiki.tga"

      Sampler "stratified" 
	      "integer xsamples" [2] "integer ysamples" [2]
	      "bool jitter" ["false"]

      PixelFilter "mitchell"

      Accelerator "grid"

      LookAt 15 7.5 -15   0 0 0   0 1 0
      Camera "perspective"
	      "float fov" [60]

      SurfaceIntegrator "whitted"
	      "integer maxdepth" [5]

      WorldBegin

      # ----------
      # Shapes
      # ----------
      AttributeBegin # bluey sky, encloses the whole scene
	      Translate 0 0 0
	      Material "matte" "color Kd" [1 1.5 3]
	      Shape "sphere" "float radius" [60] 
      AttributeEnd

      # raft body
      AttributeBegin 
	      Material "matte" "color Kd" [0.8 0.7 0.5]
	      Translate -5.4 0 0
	      Shape "cylinder" "float radius" [0.6] "float zmax" [4.5]  "float zmin" [-6.5]
	      Translate 1.2 0 0
	      Shape "cylinder" "float radius" [0.6] "float zmax" [5]  "float zmin" [-7.2]
	      Translate 1.2 0 0
	      Shape "cylinder" "float radius" [0.6] "float zmax" [5]  "float zmin" [-7.7]
	      Translate 1.2 0 0
	      Shape "cylinder" "float radius" [0.6] "float zmax" [5]  "float zmin" [-8.2]
	      Translate 1.2 0 0
	      Shape "cylinder" "float radius" [0.6] "float zmax" [5]  "float zmin" [-8.7]
	      Translate 1.2 0 0
	      Shape "cylinder" "float radius" [0.6] "float zmax" [5]  "float zmin" [-8.2]
	      Translate 1.2 0 0
	      Shape "cylinder" "float radius" [0.6] "float zmax" [5]  "float zmin" [-7.7]
	      Translate 1.2 0 0
	      Shape "cylinder" "float radius" [0.6] "float zmax" [5]  "float zmin" [-7.2]
	      Translate 1.2 0 0
	      Shape "cylinder" "float radius" [0.6] "float zmax" [4.5]  "float zmin" [-6.5]
      AttributeEnd

      # cabin
      AttributeBegin 
	      Rotate -10 1 0 0
	      Rotate 90 0 1 0
	      Translate 0 1 0
	      Scale 1 1.2 1
	      Material "matte" "color Kd" [0.8 0.1 0.1]
	      Shape "cylinder" "float radius" [2] "float zmin" [-3]  "float zmax" [3] "float phimax" [200]
      AttributeEnd

      # masts
      AttributeBegin 
	      Rotate 90 1 0 0
	      Rotate -28 0 1 0
	      Translate -3.8 -4 0 
	      Material "matte" "color Kd" [0.8 0.7 0.5]
	      Shape "cylinder" "float radius" [0.2] "float zmin" [-7.8]  "float zmax" [2.6]
      AttributeEnd
      AttributeBegin
	      Rotate 90 1 0 0
	      Rotate 28 0 1 0
	      Translate 3.8 -4 0 
	      Material "matte" "color Kd" [0.8 0.7 0.5]
	      Shape "cylinder" "float radius" [0.2] "float zmin" [-7.8]  "float zmax" [2.6]
      AttributeEnd

      # sail
      AttributeBegin 
	      Rotate 90 0 1 0
	      Translate 5.2 8 0
	      Material "matte" "color Kd" [0.8 0.7 0.5]
	      Shape "cylinder" "float radius" [0.1] "float zmin" [-3.8]  "float zmax" [3.8]
      AttributeEnd
      AttributeBegin 
	      Rotate -10 1 0 0
	      Rotate 90 0 1 0
	      Translate 0 3 0
	      Scale 1 1.2 1
	      Material "matte" "color Kd" [1 1 1]
	      Shape "cylinder" "float radius" [6] "float zmin" [-3.8]  "float zmax" [3.8] "float phimax" [50]
      AttributeEnd

      # waves
      AttributeBegin
	      Rotate 75 1 0 0
	      Rotate 90 0 1 0
	      Scale 1 1 3
	      Translate -13.5 0 0
	      Material "matte" "color Kd" [0 0 1]
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate 1 -4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate 1 -4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate 1 -4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate 1 -4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate 1 -4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -6 24 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
	      Translate -1 4 0
	      Shape "cylinder" "float radius" [13] "float zmin" [-20]  "float zmax" [20] "float phimax" [40]
      AttributeEnd

      # ----------
      # Lights
      # ----------
      AttributeBegin
	      Translate 0 20 -20
	      LightSource "point" "color I" [1200 1200 1200] 
      AttributeEnd

      AttributeBegin
	      Translate 20 20 -20
	      LightSource "point" "color I" [800 800 800]
      AttributeEnd

      AttributeBegin
	      Translate -20 -2 1
	      LightSource "point" "color I" [160 160 240]  
      AttributeEnd

      AttributeBegin
	      Translate 20 -2 1
	      LightSource "point" "color I" [260 260 320] 
      AttributeEnd

      WorldEnd
      """

  [<Fact>]
  let ``when it is read, it should return a scene object`` () =
    
    let reader = new System.IO.StringReader(input)
    let scene = SceneReader.read reader

    // TODO
    ()