namespace ``Transforms - Matrix4x4``
    open Xunit
    open Xunit.Extensions
    open FsUnit.Xunit
    open Aether.Transforms

    type ``Given an array of values`` () =
        let values = array2D [ [ 0.0f; 1.0f; 2.0f; 3.0f ]
                               [ 0.0f; 1.0f; 2.0f; 3.0f ]
                               [ 0.0f; 1.0f; 2.0f; 3.0f ]
                               [ 0.0f; 1.0f; 2.0f; 3.0f ] ]

        [<Fact>]
        let ``a Matrix4x4 can be constructed`` () =
            let matrix = Matrix4x4(values)
            matrix.Values |> should equal values

    type ``Given a tuple of values`` () =
        [<Fact>]
        let ``a Matrix4x4 can be constructed`` () =
            let matrix = Matrix4x4(1.0f, 0.0f, 0.0f, 0.0f,
                                   0.0f, 2.0f, 0.0f, 0.0f,
                                   0.0f, 0.0f, 3.0f, 0.0f,
                                   0.0f, 0.0f, 0.0f, 4.0f)
            matrix.[0, 0] |> should equal 1.0f
            matrix.[1, 1] |> should equal 2.0f
            matrix.[2, 2] |> should equal 3.0f
            matrix.[3, 3] |> should equal 4.0f

        [<Fact>]
        let ``Identity returns the identity matrix``() =
            let result = Matrix4x4.Identity
            result |> should equal (Matrix4x4(1.0f, 0.0f, 0.0f, 0.0f,
                                              0.0f, 1.0f, 0.0f, 0.0f,
                                              0.0f, 0.0f, 1.0f, 0.0f,
                                              0.0f, 0.0f, 0.0f, 1.0f))

    type ``Given a matrix`` () =
        let values = array2D [ [  3.0f; 11.0f;  4.0f;  7.0f ]
                               [  8.0f;  5.0f; 13.0f; 12.0f ]
                               [ 14.0f;  6.0f;  2.0f; 10.0f ]
                               [ 15.0f;  9.0f; 16.0f; 17.0f ] ]
        let matrix = Matrix4x4(values)

        [<Fact>]
        let ``when the indexer is called, it returns correct values``() =
            matrix.[0, 0] |> should equal  3.0f
            matrix.[0, 3] |> should equal  7.0f
            matrix.[1, 2] |> should equal 13.0f

        [<Fact>]
        let ``Transpose turns the rows of a matrix into columns and vice-versa`` () =
            let result = Matrix4x4.Transpose matrix
            result |> should equal (Matrix4x4( 3.0f,  8.0f, 14.0f, 15.0f,
                                              11.0f,  5.0f,  6.0f,  9.0f,
                                               4.0f, 13.0f,  2.0f, 16.0f,
                                               7.0f, 12.0f, 10.0f, 17.0f))

        [<Fact>]
        let ``Mul calculates the matrix product`` () =
            let matrix2 = Matrix4x4(100.0f, 101.0f, 102.0f, 103.0f,
                                    104.0f, 105.0f, 106.0f, 107.0f,
                                    108.0f, 109.0f, 110.0f, 111.0f,
                                    112.0f, 113.0f, 114.0f, 115.0f)
            let result = Matrix4x4.Mul matrix matrix2
            result |> should equal (Matrix4x4(2660.0f, 2685.0f, 2710.0f, 2735.0f,
                                              4068.0f, 4106.0f, 4144.0f, 4182.0f,
                                              3360.0f, 3392.0f, 3424.0f, 3456.0f,
                                              6068.0f, 6125.0f, 6182.0f, 6239.0f))

        [<Fact>]
        let ``Inverse calculates the matrix inverse`` () =
            let inverse = Matrix4x4.Inverse matrix
            checkMatricesRoughlyMatch inverse (Matrix4x4(-0.08f, -0.45f, -0.08f,  0.39f,
                                                         0.09f,  -0.32f, -0.10f,  0.25f,
                                                         -0.04f, -0.34f, -0.20f,  0.38f,
                                                         0.06f,   0.88f,  0.31f, -0.77f))
            checkMatricesRoughlyMatch (Matrix4x4.Mul matrix inverse) Matrix4x4.Identity


namespace ``Transforms - Transform``
    open Xunit
    open Xunit.Extensions
    open FsUnit.Xunit
    open Aether.Geometry
    open Aether.Transforms

    type ``Given a matrix and its inverse`` () =
        let matrix = Matrix4x4(3.0f,  11.0f,  4.0f,  7.0f,
                               8.0f,   5.0f, 13.0f, 12.0f,
                               14.0f,  6.0f,  2.0f, 10.0f,
                               15.0f,  9.0f, 16.0f, 17.0f)
        let matrixInverse = Matrix4x4.Inverse(matrix)

        [<Fact>]
        let ``a Transform can be constructed`` () =
            let transform = Transform(matrix, matrixInverse)
            transform.Matrix |> should equal matrix
            transform.MatrixInverse |> should equal matrixInverse

    type ``Given a matrix`` () =
        let matrix = Matrix4x4(3.0f,  11.0f,  4.0f,  7.0f,
                               8.0f,   5.0f, 13.0f, 12.0f,
                               14.0f,  6.0f,  2.0f, 10.0f,
                               15.0f,  9.0f, 16.0f, 17.0f)

        [<Fact>]
        let ``a Transform can be constructed`` () =
            let transform = Transform(matrix)
            transform.Matrix |> should equal matrix
            transform.MatrixInverse |> should equal (Matrix4x4.Inverse(matrix))

    type ``Arithmetic operators`` () =
        let matrix = Matrix4x4(3.0f,  11.0f,  4.0f,  7.0f,
                               8.0f,   5.0f, 13.0f, 12.0f,
                               14.0f,  6.0f,  2.0f, 10.0f,
                               15.0f,  9.0f, 16.0f, 17.0f)
        let transform = Transform(matrix)

        [<Fact>]
        let ``multiplication`` () =
            let expectedResult = Transform(Matrix4x4(258.0f, 175.0f, 275.0f, 312.0f,
                                                     426.0f, 299.0f, 315.0f, 450.0f,
                                                     268.0f, 286.0f, 298.0f, 360.0f,
                                                     596.0f, 459.0f, 481.0f, 662.0f))
            transform * transform |> should equal expectedResult

    type ``Factory methods`` () =

        let checkInverse (t : Transform) =
            checkMatricesRoughlyMatch (Matrix4x4.Mul t.Matrix t.MatrixInverse) Matrix4x4.Identity
        
        [<Fact>]
        let ``Translate creates a translation transform`` () =
            let result = Transform.Translate(1.5f, 2.5f, 3.5f)
            result.Matrix |> should equal (Matrix4x4(1.0f, 0.0f, 0.0f, 1.5f,
                                                     0.0f, 1.0f, 0.0f, 2.5f,
                                                     0.0f, 0.0f, 1.0f, 3.5f,
                                                     0.0f, 0.0f, 0.0f, 1.0f))
            result.MatrixInverse |> should equal (Matrix4x4(1.0f, 0.0f, 0.0f, -1.5f,
                                                            0.0f, 1.0f, 0.0f, -2.5f,
                                                            0.0f, 0.0f, 1.0f, -3.5f,
                                                            0.0f, 0.0f, 0.0f, 1.0f))
            checkInverse result

        [<Fact>]
        let ``Scale creates a scaling transform`` () =
            let result = Transform.Scale(1.5f, 2.5f, 3.5f)
            result.Matrix |> should equal (Matrix4x4(1.5f, 0.0f, 0.0f, 0.0f,
                                                     0.0f, 2.5f, 0.0f, 0.0f,
                                                     0.0f, 0.0f, 3.5f, 0.0f,
                                                     0.0f, 0.0f, 0.0f, 1.0f))
            checkInverse result

        [<Fact>]
        let ``RotateX rotates around the x axis`` () =
            let result = Transform.RotateX(60.0f)
            checkMatricesRoughlyMatch result.Matrix
                                      (Matrix4x4(1.0f, 0.0f, 0.0f, 0.0f,
                                                 0.0f, 0.5f, -0.87f, 0.0f,
                                                 0.0f, 0.87f, 0.5f, 0.0f,
                                                 0.0f, 0.0f, 0.0f, 1.0f))
            checkInverse result

        [<Fact>]
        let ``RotateY rotates around the y axis`` () =
            let result = Transform.RotateY(60.0f)
            checkMatricesRoughlyMatch result.Matrix
                                      (Matrix4x4(0.5f, 0.0f, 0.87f, 0.0f,
                                                 0.0f, 1.0f, 0.0f, 0.0f,
                                                 -0.87f, 0.0f, 0.5f, 0.0f,
                                                 0.0f, 0.0f, 0.0f, 1.0f))
            checkInverse result

        [<Fact>]
        let ``RotateZ rotates around the z axis`` () =
            let result = Transform.RotateZ(60.0f)
            checkMatricesRoughlyMatch result.Matrix
                                      (Matrix4x4(0.5f, -0.87f, 0.0f, 0.0f,
                                                 0.87f, 0.5f, 0.0f, 0.0f,
                                                 0.0f, 0.0f, 1.0f, 0.0f,
                                                 0.0f, 0.0f, 0.0f, 1.0f))
            checkInverse result

        [<Fact>]
        let ``Rotate rotates around an arbitrary axis by a given angle`` () =
            let result = Transform.Rotate 60.0f (Vector(0.5f, 0.5f, 1.0f))
            checkMatricesRoughlyMatch result.Matrix
                                      (Matrix4x4(0.58f, -0.62f,  0.52f, 0.0f,
                                                 0.79f,  0.58f, -0.19f, 0.0f,
                                                 -0.19f, 0.52f,  0.83f, 0.0f,
                                                 0.0f,   0.0f,   0.0f,  1.0f))
            checkInverse result

        [<Fact>]
        let ``LookAt places a virtual camera in a scene`` () =
            let result = Transform.LookAt(Point(1.0f, 2.0f, 3.0f),
                                          Point(4.0f, 5.0f, 6.0f),
                                          Vector.UnitY)
            checkMatricesRoughlyMatch result.Matrix
                                      (Matrix4x4(0.71f,  0.0f,  -0.71f,  1.41f,
                                                 -0.41f, 0.82f, -0.41f,  0.0f,
                                                 0.58f,  0.58f,  0.58f, -3.46f,
                                                 0.0f,   0.0f,   0.0f,   1.0f))
            checkInverse result

        [<Fact>]
        let ``Orthographic maps z values at the near plane to 0 and z values at the far plane to 1`` () =
            let result = Transform.Orthographic(2.0f, 15.0f)
            checkMatricesRoughlyMatch result.Matrix
                                      (Matrix4x4(1.0f, 0.0f, 0.0f, 0.0f,
                                                 0.0f, 1.0f, 0.0f, 0.0f,
                                                 0.0f, 0.0f, 0.08f, -0.15f,
                                                 0.0f, 0.0f, 0.0f, 1.0f))
            checkInverse result

        [<Fact>]
        let ``Perspective projects points onto a viewing plane perpendicular to the z axis`` () =
            let result = Transform.Perspective(60.0f, 2.0f, 15.0f)
            checkMatricesRoughlyMatch result.Matrix
                                      (Matrix4x4(1.73f, 0.0f, 0.0f, 0.0f,
                                                 0.0f, 1.73f, 0.0f, 0.0f,
                                                 0.0f, 0.0f, 1.15f, -2.31f,
                                                 0.0f, 0.0f, 1.0f, 0.0f))
            checkInverse result

    type ``Given an orthographic transform`` () =
        let transform = Transform.Orthographic(2.0f, 15.0f)

        [<Theory>]
        [<InlineData(0.0f, 0.0f,  1.0f, -0.08f)>]
        [<InlineData(0.0f, 0.0f,  2.0f,  0.0f)>]
        [<InlineData(0.0f, 0.0f, 15.0f,  1.0f)>]
        [<InlineData(0.0f, 0.0f, 21.0f,  1.46f)>]
        let ``when Transform is called, it transforms points correctly`` x y z expectedZ =
            let result = transform.Transform(Point(x, y, z))
            result.X |> should equal 0.0f
            result.Y |> should equal 0.0f
            result.Z |> should (equalWithin 0.01f) expectedZ

    type ``Given a perspective transform`` () =
        let transform = Transform.Perspective(60.0f, 2.0f, 15.0f)

        [<Theory>]
        [<InlineData(1.0f, 2.0f,  1.0f, 1.73f, 3.46f, -1.15f)>]
        [<InlineData(1.0f, 2.0f,  2.0f, 0.87f, 1.73f,  0.0f)>]
        [<InlineData(1.0f, 2.0f, 15.0f, 0.12f, 0.23f,  1.0f)>]
        [<InlineData(1.0f, 2.0f, 21.0f, 0.08f, 0.16f,  1.04f)>]
        let ``when Transform is called, it transforms points correctly`` x y z expectedX expectedY expectedZ =
            let result = transform.Transform(Point(x, y, z))
            result.X |> should (equalWithin 0.01f) expectedX
            result.Y |> should (equalWithin 0.01f) expectedY
            result.Z |> should (equalWithin 0.01f) expectedZ
 
    type ``Given a transform without a scaling term`` () =
        let transform = Transform.Scale(1.0f, 1.0f, 1.0f)

        [<Fact>]
        let ``when HasScale() is called, it returns false`` () =
            transform.HasScale() |> should be False

    type ``Given a transform with a scaling term`` () =
        let transform = Transform.Scale(2.0f, 1.0f, 1.0f)

        [<Fact>]
        let ``when HasScale() is called, it returns true`` () =
            transform.HasScale() |> should be True

    type ``Given a transform that does not swap handedness`` () =
        let transform = Transform.Scale(1.0f, 1.0f, 1.0f)

        [<Fact>]
        let ``when SwapsHandedness() is called, it returns false`` () =
            transform.SwapsHandedness() |> should be False

    type ``Given a transform that swaps handedness`` () =
        let transform = Transform.Scale(1.0f, -1.0f, 1.0f)

        [<Fact>]
        let ``when SwapsHandedness() is called, it returns true`` () =
            transform.SwapsHandedness() |> should be True

    type ``Given a translation transform and a rotation transform`` () =
        let translateTransform = Transform.Translate(2.0f, 3.0f, 5.0f)
        let rotateTransform    = Transform.RotateX(45.0f)

        [<Fact>]
        let ``they can transform points`` () =
            translateTransform.Transform(Point(1.0f, 2.0f, 3.0f)) |> should equal (Point(3.0f, 5.0f, 8.0f))

            let result = rotateTransform.Transform(Point(1.0f, 2.0f, 3.0f))
            result.X |> should (equalWithin 0.01f) 1.0f
            result.Y |> should (equalWithin 0.01f) -0.71f
            result.Z |> should (equalWithin 0.01f) 3.54

        [<Fact>]
        let ``they can transform vectors`` () =
            translateTransform.Transform(Vector(1.0f, 2.0f, 3.0f)) |> should equal (Vector(1.0f, 2.0f, 3.0f))
            
            let result = rotateTransform.Transform(Vector(1.0f, 2.0f, 3.0f))
            result.X |> should (equalWithin 0.01f) 1.0f
            result.Y |> should (equalWithin 0.01f) -0.71f
            result.Z |> should (equalWithin 0.01f) 3.54

        [<Fact>]
        let ``they can transform normals`` () =
            translateTransform.Transform(Normal(1.0f, 2.0f, 3.0f)) |> should equal (Normal(1.0f, 2.0f, 3.0f))

            let result = rotateTransform.Transform(Normal(1.0f, 2.0f, 3.0f))
            result.X |> should (equalWithin 0.01f) 1.0f
            result.Y |> should (equalWithin 0.01f) -0.71f
            result.Z |> should (equalWithin 0.01f) 3.54

        [<Fact>]
        let ``the translation transform can transform ray segments`` () =
            let ray = RaySegment(Point(1.0f, 2.0f, 3.0f),
                                 Vector.UnitZ,
                                 0.0f, infinityf)
            let expectedRay = RaySegment(Point(3.0f, 5.0f, 8.0f),
                                         Vector.UnitZ,
                                         0.0f, infinityf)
            translateTransform.Transform(ray) |> should equal expectedRay

        [<Fact>]
        let ``the translation transform can transform bounding boxes`` () =
            let bBox = BBox(Point(1.0f, 2.0f, 3.0f),
                            Point(7.0f, 8.0f, 9.0f))
            let expectedBBox = BBox(Point(3.0f, 5.0f, 8.0f),
                                    Point(9.0f, 11.0f, 14.0f))
            translateTransform.Transform(bBox) |> should equal expectedBBox