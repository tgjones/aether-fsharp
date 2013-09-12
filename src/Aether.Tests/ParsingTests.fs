module Aether.Tests.ParsingTests

open NUnit.Framework
open Nexus
open Aether.Parsing

module public ParserTests =

    [<Test>]
    let ``can parse LookAt directive``() =
        let result = Parser.parse "LookAt 0 10 100 0 -1 0 0 1 0"

        let expected = [
            Ast.LookAtDirective(Point3D(0.0f, 10.0f, 100.0f), 
                                Point3D(0.0f, -1.0f, 0.0f), 
                                Vector3D(0.0f, 1.0f, 0.0f))
        ]
        Assert.That(result, Is.EqualTo(expected))

    [<Test>]
    let ``can parse Film directive``() =
        let result = Parser.parse """
            Film "image" "string filename" "simple.exr"
                 "integer xresolution" [200] "integer yresolution" [200]
                 "float cropwindow" [ .2 .5 .3 .8 ]
            """

        let expected = [
            Ast.FilmDirective("image",
                [
                    ("filename", StringValue("simple.exr"))
                    ("xresolution", IntegerValue(200))
                    ("yresolution", IntegerValue(200))
                    ("cropwindow", (ArrayValue([FloatValue(0.2f)
                                                FloatValue(0.5f)
                                                FloatValue(0.3f)
                                                FloatValue(0.8f)])))
                ]
            )
        ]
        Assert.That(result, Is.EqualTo(expected))

    [<Test; Ignore>]
    let ``can parse full scene file``() =
        let result = Parser.parse """
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
            WorldEnd"""

        let expected =
            [ Ast.LookAtDirective(Point3D.Zero, Point3D.Zero, Vector3D.Zero) ]
        Assert.That(result, Is.EqualTo(expected))