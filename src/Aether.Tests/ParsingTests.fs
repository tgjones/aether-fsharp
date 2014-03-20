namespace ``Parsing - Parser``

open Xunit
open FsUnit.Xunit
open Aether
open Aether.Geometry
open Aether.Parsing

type ``Given a LookAt directive`` () =
  let input = "LookAt 0 10 100 0 -1 0 0 1 0"

  [<Fact>]
  let ``when it is parsed, it should return the correct AST`` () =
    Parser.parse input |> should equal [
      Ast.LookAt(Point(0.0f, 10.0f, 100.0f), 
                 Point(0.0f, -1.0f, 0.0f), 
                 Vector(0.0f, 1.0f, 0.0f))
    ]

type ``Given a Film directive`` () =
  let input = """
    Film "image" "string filename" "simple.exr"
            "integer xresolution" [200] "integer yresolution" [200]
            "float cropwindow" [ .2 .5 .3 .8 ]
    """

  [<Fact>]
  let ``when it is parsed, it should return the correct AST`` () =
    Parser.parse input |> should equal [
      Ast.StandardDirective(StandardDirectiveType.Film, "image",
        ParamSet([ ("filename", box "simple.exr")
                   ("xresolution", box 200)
                   ("yresolution", box 200)
                   ("cropwindow", box [ box 0.2f
                                        box 0.5f
                                        box 0.3f
                                        box 0.8f ])
        ])
      )
    ]

type ``Given a full scene file`` () =
  let input = """
      LookAt 0 10 100 0 -1 0 0 1 0
      Camera "perspective" "float fov" [30]
      PixelFilter "mitchell" "float xwidth" [2] "float ywidth" [2]
      Sampler "bestcandidate"
      Film "image" "string filename" ["simple.exr"]
              "integer xresolution" [200] "integer yresolution" [200]

      WorldBegin
      AttributeBegin
          CoordSysTransform "camera"
          LightSource "distant"
                      "point from" [0 0 0] "point to" [0 0 1]
                      "rgb L" [3 3 3]
      AttributeEnd

      AttributeBegin
          Rotate 135 1 0 0
          Texture "checks" "spectrum" "checkerboard"
                  "float uscale" [4] "float vscale" [4]
                  "rgb tex1" [1 0 0] "rgb tex2" [0 0 1]

          Material "matte"
                      "texture Kd" "checks"
          Shape "disk" "float radius" [20] "float height" [-1]
      AttributeEnd
      WorldEnd
      """

  [<Fact>]
  let ``when it is parsed, it should return the correct AST`` () =
    Parser.parse input |> should equal [
      Ast.LookAt(Point(0.0f, 10.0f, 100.0f),
                 Point(0.0f, -1.0f, 0.0f),
                 Vector(0.0f, 1.0f, 0.0f))
      Ast.StandardDirective(StandardDirectiveType.Camera, "perspective",
                            ParamSet([ ("fov", box 30.0f) ]))
      Ast.StandardDirective(StandardDirectiveType.PixelFilter, "mitchell",
                            ParamSet([ ("xwidth", box 2.0f)
                                       ("ywidth", box 2.0f) ]))
      Ast.StandardDirective(StandardDirectiveType.Sampler, "bestcandidate", ParamSet([]))
      Ast.StandardDirective(StandardDirectiveType.Film, "image", 
                            ParamSet([ ("filename", box "simple.exr");
                                       ("xresolution", box 200) 
                                       ("yresolution", box 200) ]))

      Ast.WorldBegin
      Ast.AttributeBegin

      Ast.CoordSysTransform("camera")
      Ast.StandardDirective(StandardDirectiveType.LightSource, "distant",
                            ParamSet([ ("from", box Point.Zero)
                                       ("to", box (Point(0.0f, 0.0f, 1.0f)))
                                       ("L", box (Spectrum(3.0f, 3.0f, 3.0f))) ]))

      Ast.AttributeEnd

      Ast.AttributeBegin

      Ast.Rotate(135.0f, Vector(1.0f, 0.0f, 0.0f))
      Ast.Texture("checks", "spectrum", "checkerboard",
                  ParamSet([ ("uscale", box 4.0f); 
                             ("vscale", box 4.0f); 
                             ("tex1", box (Spectrum(1.0f, 0.0f, 0.0f))); 
                             ("tex2", box (Spectrum(0.0f, 0.0f, 1.0f))) ]))
      Ast.StandardDirective(StandardDirectiveType.Material, "matte",
                            ParamSet([ ("Kd", box "checks") ]))
      Ast.StandardDirective(StandardDirectiveType.Shape, "disk",
                            ParamSet([ ("radius", box 20.0f) 
                                       ("height", box -1.0f) ]))

      Ast.AttributeEnd
      Ast.WorldEnd
    ]