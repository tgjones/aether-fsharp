namespace Aether.Transforms

open Aether.Math
open Aether.Geometry


type Matrix4x4(values : single[,]) =

    member this.Values = values

    member m.Item
        with get (i,j) = values.[i,j]

    override this.Equals(other) =
        match other with
        | :? Matrix4x4 as m2 -> values = m2.Values
        | _ -> false


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Matrix4x4 =

    let initValues (t00 : single) (t01 : single) (t02 : single) (t03 : single)
                   (t10 : single) (t11 : single) (t12 : single) (t13 : single)
                   (t20 : single) (t21 : single) (t22 : single) (t23 : single)
                   (t30 : single) (t31 : single) (t32 : single) (t33 : single) =
        let values = array2D [ [ t00; t01; t02; t03 ]
                               [ t10; t11; t12; t13 ]
                               [ t20; t21; t22; t23 ]
                               [ t30; t31; t32; t33 ] ]
        Matrix4x4(values)

    let identity = initValues 1.0f 0.0f 0.0f 0.0f
                              0.0f 1.0f 0.0f 0.0f
                              0.0f 0.0f 1.0f 0.0f
                              0.0f 0.0f 0.0f 1.0f

    let transpose (m : Matrix4x4) =
        initValues m.[0, 0] m.[1, 0] m.[2, 0] m.[3, 0]
                   m.[0, 1] m.[1, 1] m.[2, 1] m.[3, 1]
                   m.[0, 2] m.[1, 2] m.[2, 3] m.[3, 2]
                   m.[0, 3] m.[1, 3] m.[2, 3] m.[3, 3]

    let mul (m1 : Matrix4x4) (m2 : Matrix4x4) =
        let values = Array2D.zeroCreate<single> 4 4
        for i in 0..3 do
            for j in 0..3 do
                values.[i,j] <- m1.[i,0] * m2.[0,j] +
                                m1.[i,1] * m2.[1,j] +
                                m1.[i,2] * m2.[2,j] +
                                m1.[i,3] * m2.[3,j]
        Matrix4x4(values)

    let inverse (m : Matrix4x4) =

        let indxc = Array.zeroCreate<int> 4
        let indxr = Array.zeroCreate<int> 4
        let ipiv = [| 0; 0; 0; 0 |]
        let minv = Array2D.copy m.Values
        
        for i in 0..3 do
            let mutable irow = -1
            let mutable icol = -1
            let mutable big = 0.0f

            // Choose pivot.
            for j in 0..3 do
                if ipiv.[j] <> 1 then
                    for k in 0..3 do
                        if ipiv.[k] = 0 then
                            if abs minv.[j,k] >= big then
                                big <- abs minv.[j,k]
                                irow <- j
                                icol <- k
                        else if ipiv.[k] > 1 then
                            failwith "Singular error in Matrix4x4 invert."

            ipiv.[icol] <- ipiv.[icol] + 1

            // Swap rows irow and icol for pivot.
            if irow <> icol then
                for k in 0..3 do
                    swap &minv.[irow,k] &minv.[icol,k]

            indxr.[i] <- irow
            indxc.[i] <- icol
            
            if minv.[icol,icol] = 0.0f then
                failwith "Singular matrix in Matrix4x invert."

            // Set m[icol,icol] to one by scaling row icol appropriately.
            let pivinv = 1.0f / minv.[icol,icol]
            minv.[icol,icol] <- 1.0f
            for j in 0..3 do
                minv.[icol,j] <- minv.[icol,j] * pivinv

            // Subtract this row from others to zero out their columns.
            for j in 0..3 do
                if j <> icol then
                    let save = minv.[j,icol]
                    minv.[j,icol] <- 0.0f
                    for k in 0..3 do
                        minv.[j,k] <- minv.[j,k] - (minv.[icol,k] * save)

        // Swap columns to reflect permutation.
        for j in 3..0 do
            if indxr.[j] <> indxc.[j] then
                for k in 0..3 do
                    swap &minv.[k,indxr.[j]] &minv.[k,indxc.[j]]

        Matrix4x4(minv)


type Transform(matrix : Matrix4x4, matrixInverse : Matrix4x4) =
    
    new(matrix) = Transform(matrix, Matrix4x4.inverse matrix)

    member this.Matrix = matrix
    member this.MatrixInverse = matrixInverse

    static member (*) (t1 : Transform, t2 : Transform) =
        let m1 = Matrix4x4.mul t1.Matrix t2.Matrix
        let m2 = Matrix4x4.mul t2.MatrixInverse t1.MatrixInverse
        Transform(m1, m2)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Transform =

    [<CompiledName("Translate")>]
    let translate (delta : Vector) =
        let m = Matrix4x4.initValues 1.0f 0.0f 0.0f delta.X
                                     0.0f 1.0f 0.0f delta.Y
                                     0.0f 0.0f 1.0f delta.Z
                                     0.0f 0.0f 0.0f 1.0f
        let minv = Matrix4x4.initValues 1.0f 0.0f 0.0f -delta.X
                                        0.0f 1.0f 0.0f -delta.Y
                                        0.0f 0.0f 1.0f -delta.Z
                                        0.0f 0.0f 0.0f 1.0f
        Transform(m, minv)

    [<CompiledName("Scale")>]
    let scale x y z =
        let m = Matrix4x4.initValues x    0.0f 0.0f 0.0f
                                     0.0f y    0.0f 0.0f
                                     0.0f 0.0f z    0.0f
                                     0.0f 0.0f 0.0f 1.0f
        let minv = Matrix4x4.initValues (1.0f / x) 0.0f       0.0f       0.0f
                                        0.0f       (1.0f / y) 0.0f       0.0f
                                        0.0f       0.0f       (1.0f / z) 0.0f
                                        0.0f       0.0f       0.0f       1.0f
        Transform(m, minv)

    let rotateX angle =
        let sinT = sin (toRadians angle)
        let cosT = cos (toRadians angle)
        let m = Matrix4x4.initValues 1.0f 0.0f  0.0f 0.0f
                                     0.0f cosT -sinT 0.0f
                                     0.0f sinT  cosT 0.0f
                                     0.0f 0.0f  0.0f 1.0f
        Transform(m, Matrix4x4.transpose m)

    let rotateY angle =
        let sinT = sin (toRadians angle)
        let cosT = cos (toRadians angle)
        let m = Matrix4x4.initValues  cosT 0.0f sinT 0.0f
                                      0.0f 1.0f 0.0f 0.0f
                                     -sinT 0.0f cosT 0.0f
                                      0.0f 0.0f 0.0f 1.0f
        Transform(m, Matrix4x4.transpose m)

    let rotateZ angle =
        let sinT = sin (toRadians angle)
        let cosT = cos (toRadians angle)
        let m = Matrix4x4.initValues cosT -sinT 0.0f 0.0f
                                     sinT  cosT 0.0f 0.0f
                                     0.0f  0.0f 1.0f 0.0f
                                     0.0f  0.0f 0.0f 1.0f
        Transform(m, Matrix4x4.transpose m)

    let rotate angle (axis : Vector) =
        let a = Vector.normalize axis
        let s = sin (toRadians angle)
        let c = cos (toRadians angle)
        let m = Array2D.zeroCreate 4 4

        m.[0,0] <- a.X * a.X + (1.0f - a.X * a.X) * c
        m.[0,1] <- a.X * a.Y * (1.0f - c) - a.Z * s
        m.[0,2] <- a.X * a.Z * (1.0f - c) + a.Y * s
        m.[0,3] <- 0.0f

        m.[1,0] <- a.X * a.Y + (1.0f - c) + a.Z * s
        m.[1,1] <- a.Y * a.Y + (1.0f - a.Y * a.Y) * c
        m.[1,2] <- a.Y * a.Z * (1.0f - c) - a.X * s
        m.[1,3] <- 0.0f

        m.[2,0] <- a.X * a.Z * (1.0f - c) - a.Y * s
        m.[2,1] <- a.Y * a.Z * (1.0f - c) + a.X * s
        m.[2,2] <- a.Z * a.Z + (1.0f - a.Z * a.Z) * c
        m.[2,3] <- 0.0f

        m.[3,0] <- 0.0f
        m.[3,1] <- 0.0f
        m.[3,2] <- 0.0f
        m.[3,3] <- 1.0f

        let mat = Matrix4x4(m)
        Transform(mat, Matrix4x4.transpose mat)

    [<CompiledName("LookAt")>]
    let lookAt (pos : Point) (look : Point) (up : Vector) =
        let m = Array2D.zeroCreate 4 4

        // Initialize fourth column of viewing matrix.
        m.[0,3] <- pos.X
        m.[1,3] <- pos.Y
        m.[2,3] <- pos.Z
        m.[3,3] <- 1.0f

        // Initialize first three columns of viewing matrix.
        let dir = Vector.normalize (look - pos)
        let left = Vector.normalize (cross (Vector.normalize up) dir)
        let newUp = cross dir left
        m.[0,0] <- left.X
        m.[1,0] <- left.Y
        m.[2,0] <- left.Z
        m.[3,0] <- 0.0f
        m.[0,1] <- newUp.X
        m.[1,1] <- newUp.Y
        m.[2,1] <- newUp.Z
        m.[3,1] <- 0.0f
        m.[0,2] <- dir.X
        m.[1,2] <- dir.Y
        m.[2,2] <- dir.Z
        m.[3,2] <- 0.0f

        let camToWorld = Matrix4x4(m)
        Transform(Matrix4x4.inverse camToWorld, camToWorld)

    let transformPoint (p : Point) (t : Transform) =
        let x = p.X
        let y = p.Y
        let z = p.Z
        let xp = t.Matrix.[0,0]*x + t.Matrix.[0,1]*y + t.Matrix.[0,2]*z + t.Matrix.[0,3]
        let yp = t.Matrix.[1,0]*x + t.Matrix.[1,1]*y + t.Matrix.[1,2]*z + t.Matrix.[1,3]
        let zp = t.Matrix.[2,0]*x + t.Matrix.[2,1]*y + t.Matrix.[2,2]*z + t.Matrix.[2,3]
        let wp = t.Matrix.[3,0]*x + t.Matrix.[3,1]*y + t.Matrix.[3,2]*z + t.Matrix.[3,3]
        if wp = 1.0f then Point(xp, yp, zp)
        else Point(xp, yp, zp) / wp

    let transformBBox (b : BBox) (transform : Transform) =
        let mutable ret = BBox.fromPoint(transformPoint (Point(b.Min.X, b.Min.Y, b.Min.Z)) transform)
        ret <- BBox.unionBoxPoint ret   (transformPoint (Point(b.Max.X, b.Min.Y, b.Min.Z)) transform)
        ret <- BBox.unionBoxPoint ret   (transformPoint (Point(b.Min.X, b.Max.Y, b.Min.Z)) transform)
        ret <- BBox.unionBoxPoint ret   (transformPoint (Point(b.Min.X, b.Min.Y, b.Max.Z)) transform)
        ret <- BBox.unionBoxPoint ret   (transformPoint (Point(b.Min.X, b.Max.Y, b.Max.Z)) transform)
        ret <- BBox.unionBoxPoint ret   (transformPoint (Point(b.Max.X, b.Max.Y, b.Min.Z)) transform)
        ret <- BBox.unionBoxPoint ret   (transformPoint (Point(b.Max.X, b.Min.Y, b.Max.Z)) transform)
        ret <- BBox.unionBoxPoint ret   (transformPoint (Point(b.Max.X, b.Max.Y, b.Max.Z)) transform)
        ret

    let transformVector (p : Vector) (t : Transform) =
        let x = p.X
        let y = p.Y
        let z = p.Z
        Vector(t.Matrix.[0,0]*x + t.Matrix.[0,1]*y + t.Matrix.[0,2]*z,
               t.Matrix.[1,0]*x + t.Matrix.[1,1]*y + t.Matrix.[1,2]*z,
               t.Matrix.[2,0]*x + t.Matrix.[2,1]*y + t.Matrix.[2,2]*z)

    let transformNormal (p : Normal) (t : Transform) =
        let x = p.X
        let y = p.Y
        let z = p.Z
        Vector(t.MatrixInverse.[0,0]*x + t.MatrixInverse.[1,0]*y + t.MatrixInverse.[2,0]*z,
               t.MatrixInverse.[0,1]*x + t.MatrixInverse.[1,1]*y + t.MatrixInverse.[2,1]*z,
               t.MatrixInverse.[0,2]*x + t.MatrixInverse.[1,2]*y + t.MatrixInverse.[2,2]*z)

    let applyr (r : RaySegment) (t : Transform) =
        RaySegment(t |> transformPoint r.Origin, t |> transformVector r.Direction,
                   r.MinT, r.MaxT, r.Time)

    let swapsHandedness (t : Transform) =
        let det = ((t.Matrix.[0,0] *
                    (t.Matrix.[1,1] * t.Matrix.[2,2] -
                     t.Matrix.[1,2] * t.Matrix.[2,1])) -
                   (t.Matrix.[0,1] *
                    (t.Matrix.[1,0] * t.Matrix.[2,2] -
                     t.Matrix.[1,2] * t.Matrix.[2,0])) +
                   (t.Matrix.[0,2] *
                    (t.Matrix.[1,0] * t.Matrix.[2,1] -
                     t.Matrix.[1,1] * t.Matrix.[2,0])));
        det < 0.0f

    let orthographic zNear zFar =
        (scale 1.0f 1.0f (1.0f / (zFar - zNear))) *
        (translate (Vector(0.0f, 0.0f, -zNear)))

    let perspective fov n f =
        // Perform perspective divide.
        let persp = Matrix4x4.initValues 1.0f 0.0f          0.0f             0.0f
                                         0.0f 1.0f          0.0f             0.0f
                                         0.0f 0.0f (f / (f - n)) (-f*n / (f - n))
                                         0.0f 0.0f          1.0f             0.0f

        // Scale to canonical viewing volume.
        let invTanAng = 1.0f / (tan ((toRadians fov) / 2.0f))
        (scale invTanAng invTanAng 1.0f) * Transform(persp)

    let hasScale (t : Transform) =
        let la2 = t |> transformVector (Vector(1.0f, 0.0f, 0.0f)) |> Vector.lengthSq
        let lb2 = t |> transformVector (Vector(0.0f, 1.0f, 0.0f)) |> Vector.lengthSq
        let lc2 = t |> transformVector (Vector(0.0f, 0.0f, 1.0f)) |> Vector.lengthSq
        let notOne x = x < 0.999f || x > 1.001f
        notOne la2 || notOne lb2 || notOne lc2

    let inverse (t : Transform) =
        Transform(t.MatrixInverse, t.Matrix)

    let transpose (t : Transform) =
        Transform(Matrix4x4.transpose t.Matrix, Matrix4x4.transpose t.MatrixInverse)


[<AutoOpen>]
module CustomOperators =
    let inline (|>>) (transform : Transform) (value : ^a) : 'a =
        match box value with
        | :? Vector as v -> unbox<'a> (Transform.transformVector v transform)
        | :? Point as p  -> unbox<'a> (Transform.transformPoint p transform)
        | :? Normal as n -> unbox<'a> (Transform.transformNormal n transform)
        | :? BBox as b   -> unbox<'a> (Transform.transformBBox b transform)
        | _ -> failwith "Can't transform this type"