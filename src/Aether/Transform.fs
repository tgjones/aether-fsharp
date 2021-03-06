﻿namespace Aether.Transforms

open Aether.Math
open Aether.Geometry


/// 4x4 matrix.
type Matrix4x4(values : single[,]) =

    member this.Values = values

    member m.Item
        with get (i,j) = values.[i,j]

    /// Creates a matrix from the given values. Often more convenient than
    /// calling the constructor with a 2D array.
    new(t00, t01, t02, t03,
        t10, t11, t12, t13,
        t20, t21, t22, t23,
        t30, t31, t32, t33) =
        Matrix4x4(array2D [ [ t00; t01; t02; t03 ]
                            [ t10; t11; t12; t13 ]
                            [ t20; t21; t22; t23 ]
                            [ t30; t31; t32; t33 ] ])

    /// Returns the identity matrix. The identity matrix has ones on the main
    /// diagonal, and zeros elsewhere.
    static member Identity = Matrix4x4(1.0f, 0.0f, 0.0f, 0.0f,
                                       0.0f, 1.0f, 0.0f, 0.0f,
                                       0.0f, 0.0f, 1.0f, 0.0f,
                                       0.0f, 0.0f, 0.0f, 1.0f)

    /// Returns the empty matrix. The identity matrix has all zeroes.
    static member Empty = Matrix4x4(0.0f, 0.0f, 0.0f, 0.0f,
                                    0.0f, 0.0f, 0.0f, 0.0f,
                                    0.0f, 0.0f, 0.0f, 0.0f,
                                    0.0f, 0.0f, 0.0f, 0.0f)

    /// Turns the rows of a given matrix into columns and vice-versa.
    static member Transpose (m : Matrix4x4) =
        Matrix4x4(m.[0, 0], m.[1, 0], m.[2, 0], m.[3, 0],
                  m.[0, 1], m.[1, 1], m.[2, 1], m.[3, 1],
                  m.[0, 2], m.[1, 2], m.[2, 2], m.[3, 2],
                  m.[0, 3], m.[1, 3], m.[2, 3], m.[3, 3])

    /// Calculates the matrix product of two given matrices.
    static member Mul (m1 : Matrix4x4) (m2 : Matrix4x4) =
        let values = Array2D.zeroCreate<single> 4 4
        for i in 0..3 do
            for j in 0..3 do
                values.[i,j] <- m1.[i,0] * m2.[0,j] +
                                m1.[i,1] * m2.[1,j] +
                                m1.[i,2] * m2.[2,j] +
                                m1.[i,3] * m2.[3,j]
        Matrix4x4(values)

    /// Inverts a given matrix using a numerically stable Gauss-Jordan
    /// elimination route. The matrix inverse, also known as the reciprocal,
    /// is calculated such that M * MInv = I, where I is the identity matrix.
    static member Inverse (m : Matrix4x4) =
        let indxc = Array.zeroCreate<int> 4
        let indxr = Array.zeroCreate<int> 4
        let ipiv =  Array.zeroCreate<int> 4
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
        for j = 3 downto 0 do
            if indxr.[j] <> indxc.[j] then
                for k in 0..3 do
                    swap &minv.[k,indxr.[j]] &minv.[k,indxc.[j]]

        Matrix4x4(minv)

    static member SolveLinearSystem2x2(A : single list list, B : single list) =
        let det = A.[0].[0] * A.[1].[1] - A.[0].[1] * A.[1].[0]
        if abs det < 1e-10f then
            None
        else
            let x0 = (A.[1].[1]*B.[0] - A.[0].[1]*B.[1]) / det
            let x1 = (A.[0].[0]*B.[1] - A.[1].[0]*B.[0]) / det
            if System.Single.IsNaN(x0) || System.Single.IsNaN(x1) then
                None
            else
                Some(x0, x1)

    override this.Equals(other) =
        match other with
        | :? Matrix4x4 as m2 -> values = m2.Values
        | _ -> false

    override this.GetHashCode() =
        values.GetHashCode()

    override this.ToString() =
        sprintf "{M00:%f M01:%f M02:%f M03:%f} {M10:%f M11:%f M12:%f M13:%f} {M20:%f M21:%f M22:%f M23:%f} {M30:%f M31:%f M32:%f M33:%f}"
                this.[0,0] this.[0,1] this.[0,2] this.[0,3]
                this.[1,0] this.[1,1] this.[1,2] this.[1,3]
                this.[2,0] this.[2,1] this.[2,2] this.[2,3]
                this.[3,0] this.[3,1] this.[3,2] this.[3,3]


/// Container for a matrix and its inverse.
type Transform(matrix : Matrix4x4, matrixInverse : Matrix4x4) =
    
    new(matrix) = Transform(matrix, Matrix4x4.Inverse matrix)
    new() = Transform(Matrix4x4.Identity, Matrix4x4.Identity)

    member this.Matrix = matrix
    member this.MatrixInverse = matrixInverse

    /// Combines the two transforms. Note that the second transform will be
    /// applied first.
    static member (*) (t1 : Transform, t2 : Transform) =
        let m1 = Matrix4x4.Mul t1.Matrix t2.Matrix
        let m2 = Matrix4x4.Mul t2.MatrixInverse t1.MatrixInverse
        Transform(m1, m2)

    /// Creates a translation transform.
    static member Translate(x, y, z) = 
        let m = Matrix4x4(1.0f, 0.0f, 0.0f, x,
                          0.0f, 1.0f, 0.0f, y,
                          0.0f, 0.0f, 1.0f, z,
                          0.0f, 0.0f, 0.0f, 1.0f)
        let minv = Matrix4x4(1.0f, 0.0f, 0.0f, -x,
                             0.0f, 1.0f, 0.0f, -y,
                             0.0f, 0.0f, 1.0f, -z,
                             0.0f, 0.0f, 0.0f, 1.0f)
        Transform(m, minv)

    /// Creates a translation transform.
    static member Translate(delta : Vector) =
        Transform.Translate(delta.X, delta.Y, delta.Z)

    /// Creates a scaling transform.
    static member Scale(x, y, z) =
        let m = Matrix4x4(x,    0.0f, 0.0f, 0.0f,
                          0.0f, y,    0.0f, 0.0f,
                          0.0f, 0.0f, z,    0.0f,
                          0.0f, 0.0f, 0.0f, 1.0f)
        let minv = Matrix4x4(1.0f / x, 0.0f,     0.0f,     0.0f,
                             0.0f,     1.0f / y, 0.0f,     0.0f,
                             0.0f,     0.0f,     1.0f / z, 0.0f,
                             0.0f,     0.0f,     0.0f,     1.0f)
        Transform(m, minv)

    /// Creates a scaling transform.
    static member Scale(values : Vector) =
        Transform.Scale(values.X, values.Y, values.Z)

    /// Creates a transform that rotates by a given angle around the x axis.
    static member RotateX angle =
        let sinT = sin (toRadians angle)
        let cosT = cos (toRadians angle)
        let m = Matrix4x4(1.0f, 0.0f,  0.0f, 0.0f,
                          0.0f, cosT, -sinT, 0.0f,
                          0.0f, sinT,  cosT, 0.0f,
                          0.0f, 0.0f,  0.0f, 1.0f)
        Transform(m, Matrix4x4.Transpose m)

    /// Creates a transform that rotates by a given angle around the y axis.
    static member RotateY angle =
        let sinT = sin (toRadians angle)
        let cosT = cos (toRadians angle)
        let m = Matrix4x4(cosT,  0.0f, sinT, 0.0f,
                          0.0f,  1.0f, 0.0f, 0.0f,
                          -sinT, 0.0f, cosT, 0.0f,
                          0.0f,  0.0f, 0.0f, 1.0f)
        Transform(m, Matrix4x4.Transpose m)

    /// Creates a transform that rotates by a given angle around the z axis.
    static member RotateZ angle =
        let sinT = sin (toRadians angle)
        let cosT = cos (toRadians angle)
        let m = Matrix4x4(cosT, -sinT, 0.0f, 0.0f,
                          sinT,  cosT, 0.0f, 0.0f,
                          0.0f,  0.0f, 1.0f, 0.0f,
                          0.0f,  0.0f, 0.0f, 1.0f)
        Transform(m, Matrix4x4.Transpose m)

    /// Creates a transform that rotates by a given angle around a given axis.
    static member Rotate angle (axis : Vector) =
        let a = Vector.Normalize axis
        let s = sin (toRadians angle)
        let c = cos (toRadians angle)
        let m = Array2D.zeroCreate 4 4

        m.[0,0] <- a.X * a.X + (1.0f - a.X * a.X) * c
        m.[0,1] <- a.X * a.Y * (1.0f - c) - a.Z * s
        m.[0,2] <- a.X * a.Z * (1.0f - c) + a.Y * s
        m.[0,3] <- 0.0f

        m.[1,0] <- a.X * a.Y * (1.0f - c) + a.Z * s
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
        Transform(mat, Matrix4x4.Transpose mat)

    /// Creates a transform that places a virtual camera in a scene at a given
    /// position, looking at a given point, with a given "up" vector that 
    /// orients the camera along the viewing direction implied by the first
    /// two parameters.
    static member LookAt(pos : Point, look : Point, up : Vector) =
        let m = Array2D.zeroCreate 4 4

        // Initialize fourth column of viewing matrix.
        m.[0,3] <- pos.X
        m.[1,3] <- pos.Y
        m.[2,3] <- pos.Z
        m.[3,3] <- 1.0f

        // Initialize first three columns of viewing matrix.
        let dir = (look - pos) |> Vector.Normalize
        let left = Vector.Cross(Vector.Normalize(up), dir) |> Vector.Normalize
        let newUp = Vector.Cross(dir, left)
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
        Transform(Matrix4x4.Inverse camToWorld, camToWorld)

    /// Transforms the given point.
    member this.Transform(p : Point) =
        let x = p.X
        let y = p.Y
        let z = p.Z
        let xp = matrix.[0,0]*x + matrix.[0,1]*y + matrix.[0,2]*z + matrix.[0,3]
        let yp = matrix.[1,0]*x + matrix.[1,1]*y + matrix.[1,2]*z + matrix.[1,3]
        let zp = matrix.[2,0]*x + matrix.[2,1]*y + matrix.[2,2]*z + matrix.[2,3]
        let wp = matrix.[3,0]*x + matrix.[3,1]*y + matrix.[3,2]*z + matrix.[3,3]
        if wp = 1.0f then Point(xp, yp, zp)
        else Point(xp, yp, zp) / wp

    /// Transforms the given bounding box.
    member this.Transform(b : BBox) =
        let mutable ret = BBox.FromPoint(this.Transform(Point(b.Min.X, b.Min.Y, b.Min.Z)))
        ret <- BBox.Union(ret, this.Transform(Point(b.Max.X, b.Min.Y, b.Min.Z)))
        ret <- BBox.Union(ret, this.Transform(Point(b.Min.X, b.Max.Y, b.Min.Z)))
        ret <- BBox.Union(ret, this.Transform(Point(b.Min.X, b.Min.Y, b.Max.Z)))
        ret <- BBox.Union(ret, this.Transform(Point(b.Min.X, b.Max.Y, b.Max.Z)))
        ret <- BBox.Union(ret, this.Transform(Point(b.Max.X, b.Max.Y, b.Min.Z)))
        ret <- BBox.Union(ret, this.Transform(Point(b.Max.X, b.Min.Y, b.Max.Z)))
        ret <- BBox.Union(ret, this.Transform(Point(b.Max.X, b.Max.Y, b.Max.Z)))
        ret

    /// Transforms the given vector.
    member this.Transform(p : Vector) =
        let x = p.X
        let y = p.Y
        let z = p.Z
        Vector(matrix.[0,0]*x + matrix.[0,1]*y + matrix.[0,2]*z,
               matrix.[1,0]*x + matrix.[1,1]*y + matrix.[1,2]*z,
               matrix.[2,0]*x + matrix.[2,1]*y + matrix.[2,2]*z)

    /// Transforms the given normal.
    member this.Transform(p : Normal) =
        let x = p.X
        let y = p.Y
        let z = p.Z
        Normal(matrixInverse.[0,0]*x + matrixInverse.[1,0]*y + matrixInverse.[2,0]*z,
               matrixInverse.[0,1]*x + matrixInverse.[1,1]*y + matrixInverse.[2,1]*z,
               matrixInverse.[0,2]*x + matrixInverse.[1,2]*y + matrixInverse.[2,2]*z)

    /// Transforms the given ray segment.
    member this.Transform(r : RaySegment) =
        RaySegment(this.Transform(r.Origin), this.Transform(r.Direction),
                   r.MinT, r.MaxT, r.Time)

    /// Returns true if the transform changes a left-handed coordinate system
    /// into a right-handed one, or vice-versa.
    member this.SwapsHandedness() =
        let det = ((matrix.[0,0] *
                    (matrix.[1,1] * matrix.[2,2] -
                     matrix.[1,2] * matrix.[2,1])) -
                   (matrix.[0,1] *
                    (matrix.[1,0] * matrix.[2,2] -
                     matrix.[1,2] * matrix.[2,0])) +
                   (matrix.[0,2] *
                    (matrix.[1,0] * matrix.[2,1] -
                     matrix.[1,1] * matrix.[2,0])));
        det < 0.0f

    /// Maps z values at the near plane to 0 and z values at the far plane to 1.
    static member Orthographic(zNear, zFar) =
        Transform.Scale(1.0f, 1.0f, 1.0f / (zFar - zNear)) *
        Transform.Translate(0.0f, 0.0f, -zNear)

    /// Maps z values at the near plane to 0 and z values at the far plane to 1,
    /// and scales x and y values on the projection plane based on the given
    /// field of view.
    static member Perspective(fov, n, f) =
        // Perform perspective divide.
        let persp = Matrix4x4(1.0f, 0.0f, 0.0f,        0.0f,
                              0.0f, 1.0f, 0.0f,        0.0f,
                              0.0f, 0.0f, f / (f - n), -f*n / (f - n),
                              0.0f, 0.0f, 1.0f,        0.0f)

        // Scale to canonical viewing volume.
        let invTanAng = 1.0f / (tan ((toRadians fov) / 2.0f))
        Transform.Scale(invTanAng, invTanAng, 1.0f) * Transform(persp)

    /// Returns true if the transform has a scaling term in it.
    member this.HasScale() =
        // Transform each of the three coordinate axes and see if any of their
        // lengths are noticeably different from one.
        let la2 = this.Transform(Vector.UnitX).LengthSquared()
        let lb2 = this.Transform(Vector.UnitY).LengthSquared()
        let lc2 = this.Transform(Vector.UnitZ).LengthSquared()
        let inline notOne x = x < 0.999f || x > 1.001f
        notOne la2 || notOne lb2 || notOne lc2

    /// Creates a transform that is the inverse of the given transform.
    static member Inverse (t : Transform) =
        Transform(t.MatrixInverse, t.Matrix)

    /// Creates a transform that is the transpose of the given transform.
    static member Transpose (t : Transform) =
        Transform(Matrix4x4.Transpose(t.Matrix), Matrix4x4.Transpose(t.MatrixInverse))

    override this.Equals(other) =
        match other with
        | :? Transform as t2 -> matrix = t2.Matrix
        | _ -> false

    override this.GetHashCode() =
        matrix.GetHashCode()

    override this.ToString() =
        matrix.ToString()


[<AutoOpen>]
module CustomOperators =
    let inline (|>>) (transform : ^a when ^a : (member Transform : ^b -> ^b)) (value : ^b) : 'b =
        (^a : (member Transform : ^b -> ^b) transform, value)