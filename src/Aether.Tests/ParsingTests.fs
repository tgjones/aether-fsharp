namespace ``Parsing - Parser``

open Xunit
open FsUnit.Xunit
open Nexus
open Nexus.Graphics.Colors
open Aether.Parsing

type ``Given a LookAt directive`` () =
    let input = "LookAt 0 10 100 0 -1 0 0 1 0"

    [<Fact>]
    let ``when it is parsed, it should return the correct AST`` () =
        Parser.parse input |> should equal [
            Ast.LookAt(Point3D(0.0f, 10.0f, 100.0f), 
                       Point3D(0.0f, -1.0f, 0.0f), 
                       Vector3D(0.0f, 1.0f, 0.0f))
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
                Some([ ("filename", StringValue("simple.exr"))
                       ("xresolution", IntegerValue(200))
                       ("yresolution", IntegerValue(200))
                       ("cropwindow", (ArrayValue([FloatValue(0.2f)
                                                   FloatValue(0.5f)
                                                   FloatValue(0.3f)
                                                   FloatValue(0.8f)])))
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
            Ast.LookAt(Point3D(0.0f, 10.0f, 100.0f), Point3D(0.0f, -1.0f, 0.0f), Vector3D(0.0f, 1.0f, 0.0f))
            Ast.StandardDirective(StandardDirectiveType.Camera, "perspective", Some([ ("fov", FloatValue(30.0f)) ]))
            Ast.StandardDirective(StandardDirectiveType.PixelFilter, "mitchell", Some([ ("xwidth", FloatValue(2.0f)); ("ywidth", FloatValue(2.0f)) ]))
            Ast.StandardDirective(StandardDirectiveType.Sampler, "bestcandidate", None)
            Ast.StandardDirective(StandardDirectiveType.Film, "image", Some([ ("filename", StringValue("simple.exr")); ("xresolution", IntegerValue(200)); ("yresolution", IntegerValue(200)) ]))

            Ast.WorldBegin
            Ast.AttributeBegin

            Ast.CoordSysTransform("camera")
            Ast.StandardDirective(StandardDirectiveType.LightSource, "distant", Some([ ("from", PointValue(Point3D.Zero)); ("to", PointValue(Point3D(0.0f, 0.0f, 1.0f))); ("L", ColorValue(ColorF(3.0f, 3.0f, 3.0f))) ]))

            Ast.AttributeEnd

            Ast.AttributeBegin

            Ast.Rotate(135.0f, Vector3D(1.0f, 0.0f, 0.0f))
            Ast.Texture("checks", "spectrum", "checkerboard", Some([ ("uscale", FloatValue(4.0f)); ("vscale", FloatValue(4.0f)); ("tex1", ColorValue(ColorF(1.0f, 0.0f, 0.0f))); ("tex2", ColorValue(ColorF(0.0f, 0.0f, 1.0f))) ]))
            Ast.StandardDirective(StandardDirectiveType.Material, "matte", Some([ ("Kd", StringValue("checks")) ]))
            Ast.StandardDirective(StandardDirectiveType.Shape, "disk", Some([ ("radius", FloatValue(20.0f)); ("height", FloatValue(-1.0f)) ]))

            Ast.AttributeEnd
            Ast.WorldEnd
        ]