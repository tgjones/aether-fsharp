namespace ``Transforms - Matrix4x4``

    open NHamcrest

    open Xunit
    open Xunit.Extensions
    open FsUnit.Xunit
    open Aether.Transforms

    [<AutoOpen>]
    module MatrixMatchers =
        let checkArraysRoughlyMatch (m1 : Matrix4x4) (m2 : Matrix4x4) =
            for i in 0..3 do
                for j in 0..3 do
                    m1.[i,j] |> should (equalWithin 0.01f) m2.[i,j]

    type ``Given an array of values`` () =
        let values = array2D [ [ 0.0f; 1.0f; 2.0f; 3.0f ]
                               [ 0.0f; 1.0f; 2.0f; 3.0f ]
                               [ 0.0f; 1.0f; 2.0f; 3.0f ]
                               [ 0.0f; 1.0f; 2.0f; 3.0f ] ]

        [<Fact>]
        let ``a Matrix4x4 can be constructed``() =
            let matrix = Matrix4x4(values)
            matrix.Values |> should equal values

    type ``Given the Matrix4x4 type`` () =
        [<Fact>]
        let ``FromValues creates a new instance``() =
            let matrix = Matrix4x4.FromValues 1.0f 0.0f 0.0f 0.0f
                                              0.0f 2.0f 0.0f 0.0f
                                              0.0f 0.0f 3.0f 0.0f
                                              0.0f 0.0f 0.0f 4.0f
            matrix.[0, 0] |> should equal 1.0f
            matrix.[1, 1] |> should equal 2.0f
            matrix.[2, 2] |> should equal 3.0f
            matrix.[3, 3] |> should equal 4.0f

        [<Fact>]
        let ``Identity returns the identity matrix``() =
            let result = Matrix4x4.Identity
            result |> should equal (Matrix4x4.FromValues 1.0f 0.0f 0.0f 0.0f
                                                         0.0f 1.0f 0.0f 0.0f
                                                         0.0f 0.0f 1.0f 0.0f
                                                         0.0f 0.0f 0.0f 1.0f)

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
            result |> should equal (Matrix4x4.FromValues  3.0f  8.0f 14.0f 15.0f
                                                         11.0f  5.0f  6.0f  9.0f
                                                          4.0f 13.0f  2.0f 16.0f
                                                          7.0f 12.0f 10.0f 17.0f)

        [<Fact>]
        let ``Mul calculates the matrix product`` () =
            let matrix2 = Matrix4x4.FromValues 100.0f 101.0f 102.0f 103.0f
                                               104.0f 105.0f 106.0f 107.0f
                                               108.0f 109.0f 110.0f 111.0f
                                               112.0f 113.0f 114.0f 115.0f
            let result = Matrix4x4.Mul matrix matrix2
            result |> should equal (Matrix4x4.FromValues 2660.0f 2685.0f 2710.0f 2735.0f
                                                         4068.0f 4106.0f 4144.0f 4182.0f
                                                         3360.0f 3392.0f 3424.0f 3456.0f
                                                         6068.0f 6125.0f 6182.0f 6239.0f)

        [<Fact>]
        let ``Inverse calculates the matrix inverse`` () =
            let inverse = Matrix4x4.Inverse matrix
            checkArraysRoughlyMatch inverse (Matrix4x4.FromValues -0.08f -0.45f -0.08f  0.39f
                                                                   0.09f -0.32f -0.10f  0.25f
                                                                  -0.04f -0.34f -0.20f  0.38f
                                                                   0.06f  0.88f  0.31f -0.77f)
            checkArraysRoughlyMatch (Matrix4x4.Mul matrix inverse) Matrix4x4.Identity