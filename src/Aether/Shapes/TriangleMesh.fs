namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


type TriangleMesh(objectToWorld : Transform, reverseOrientation, numTriangles, numVertices, 
                  vertexIndices : int[],
                  points : Point list,
                  normals : Normal list,
                  tangents : Vector list,
                  textureCoordinates : TextureCoordinate list,
                  alphaTexture : ITexture<single> option) =
    inherit Shape(objectToWorld, reverseOrientation)

    // Transform mesh vertices to world space.
    let worldSpacePoints = points |> List.map (fun x -> objectToWorld |>> x)

    let objectSpaceBounds = BBox.FromPoints(points)
    let worldSpaceBounds = BBox.FromPoints(worldSpacePoints)

    member this.VertexIndices = vertexIndices
    member this.Points = worldSpacePoints
    member this.Normals = normals
    member this.Tangents = tangents
    member this.TextureCoordinates = textureCoordinates
    member this.AlphaTexture = alphaTexture

    override this.ObjectSpaceBounds = objectSpaceBounds
    override this.WorldSpaceBounds = worldSpaceBounds
    override this.CanIntersect = false

    override this.Refine () =
        [ for n in [0 .. numTriangles-1] ->
              Triangle(objectToWorld, reverseOrientation, this, n) :> Shape ]


and Triangle(objectToWorld, reverseOrientation, mesh : TriangleMesh, n) =
    inherit Shape(objectToWorld, reverseOrientation)

    let vertexIndices = mesh.VertexIndices.[n..n+2]

    // Get triangle vertices in p1, p2, p3
    let p1 = mesh.Points.[vertexIndices.[0]]
    let p2 = mesh.Points.[vertexIndices.[1]]
    let p3 = mesh.Points.[vertexIndices.[2]]

    // Get texture coordinates
    let uvs =
        if mesh.TextureCoordinates.Length <> 0 then
            [ mesh.TextureCoordinates.[vertexIndices.[0]]
              mesh.TextureCoordinates.[vertexIndices.[1]]
              mesh.TextureCoordinates.[vertexIndices.[2]] ]
        else
            [ { X = 0.0f; Y = 0.0f }
              { X = 1.0f; Y = 0.0f }
              { X = 0.0f; Y = 1.0f } ]

    override this.ObjectSpaceBounds =
        BBox.FromPoints [ this.WorldToObject |>> p1
                          this.WorldToObject |>> p2
                          this.WorldToObject |>> p3 ]

    override this.WorldSpaceBounds =
        BBox.FromPoints [ p1; p2; p3 ]

    override this.TryIntersect(ray) =
        let e1 = p2 - p1
        let e2 = p3 - p1
        let s1 = Vector.Cross(ray.Direction, e1)
        let divisor = Vector.Dot(s1, e1)

        if divisor = 0.0f then None
        else
            let invDivisor = 1.0f / divisor

            // Compute first barycentric coordinate
            let d = ray.Origin - p1
            let b1 = Vector.Dot(d, s1) * invDivisor
            if b1 < 0.0f || b1 > 1.0f then None
            else    
                // Compute second barycentric coordinate
                let s2 = Vector.Cross(d, e1)
                let b2 = Vector.Dot(ray.Direction, s2) * invDivisor
                if b2 < 0.0f || b1 + b2 > 1.0f then None
                else
                    // Compute t to intersection point
                    let t = Vector.Dot(e2, s2) * invDivisor
                    if t < ray.MinT || t > ray.MaxT then None
                    else    
                        // Compute deltas for triangle partial derivatives
                        let du1 = uvs.[0].X - uvs.[2].X
                        let du2 = uvs.[1].X - uvs.[2].X
                        let dv1 = uvs.[0].Y - uvs.[2].Y
                        let dv2 = uvs.[1].Y - uvs.[2].Y
                        let dp1 = p1 - p3
                        let dp2 = p2 - p3
                        let determinant = du1 * dv2 - dv1 * du2
                        let dpdu, dpdv =
                            if determinant = 0.0f then
                                // Handle zero determinant for triangle partial derivative matrix
                                failwith "Not done yet"
                            else
                                let invdet = 1.0f / determinant
                                ( ( dv2 * dp1 - dv1 * dp2) * invdet,
                                  (-du2 * dp1 + du1 * dp2) * invdet )

                        // Interpolate $(u,v)$ triangle parametric coordinates
                        let b0 = 1.0f - b1 - b2
                        let tu = b0 * uvs.[0].X + b1 * uvs.[1].X + b2 * uvs.[2].X
                        let tv = b0 * uvs.[0].Y + b1 * uvs.[1].Y + b2 * uvs.[2].Y

                        // Fill in DifferentialGeometry from triangle hit
                        let dg = DifferentialGeometry(ray.Evaluate t, dpdu, dpdv,
                                                      Normal.Zero, Normal.Zero,
                                                      tu, tv, this)
                        let rayEpsilon = 1e-3f * t

                        // Test intersection against alpha texture, if present.
                        if ray.Depth <> -1 && mesh.AlphaTexture.IsSome && mesh.AlphaTexture.Value.Evaluate(dg) = 0.0f then
                            None
                        else
                            Some(t, rayEpsilon, dg)


    override this.Area() =
        0.5f * Vector.Cross(p2-p1, p3-p1).Length()

    override this.GetShadingGeometry(objectToWorld, dg) =
        if mesh.Normals.Length = 0 && mesh.Tangents.Length = 0 then
            dg
        else
            // Initialize Triangle shading geometry with normals and tangents.

            // Compute barycentric coordinates for point.
            // Initialize A and C matrices for barycentrics.
            let A = [ [ uvs.[1].X - uvs.[0].X]; [ uvs.[2].X - uvs.[0].X ]
                      [ uvs.[1].Y - uvs.[0].Y]; [ uvs.[2].Y - uvs.[0].Y ] ]
            let C = [ dg.U - uvs.[0].X; dg.V - uvs.[0].Y ]

            let b =
                match Matrix4x4.SolveLinearSystem2x2(A, C) with
                | Some(b1, b2) -> [ 1.0f - b1 - b2; b1; b2 ]
                | None ->
                    // Handle degenerate parametric mapping
                    [ 1.0f / 3.0f; 1.0f / 3.0f; 1.0f / 3.0f ]

            // Use n and s to compute shading tangents for triangle, ss and ts.
            let ns =
                if mesh.Normals.Length <> 0 then
                    objectToWorld.Transform(b.[0] * mesh.Normals.[vertexIndices.[0]] +
                                            b.[1] * mesh.Normals.[vertexIndices.[1]] +
                                            b.[2] * mesh.Normals.[vertexIndices.[2]])
                    |> Normal.Normalize
                else
                    dg.Normal

            let ss' =
                if mesh.Tangents.Length <> 0 then
                    objectToWorld.Transform(b.[0] * mesh.Tangents.[vertexIndices.[0]] +
                                            b.[1] * mesh.Tangents.[vertexIndices.[1]] +
                                            b.[2] * mesh.Tangents.[vertexIndices.[2]])
                    |> Vector.Normalize
                else
                    dg.DpDu |> Vector.Normalize

            let ts' = Vector.Cross(ss', ns)
            let ts, ss =
                if ts'.LengthSquared() > 0.0f then
                    let ts = ts' |> Vector.Normalize
                    let ss = Vector.Cross(ts, ns)
                    ts, ss
                else
                    let ss, ts = Vector.CoordinateSystem(ns.ToVector())
                    ts, ss

            // Compute dndu and dndv for triangle shading geometry.
            let dndu, dndv =
                if mesh.Normals.Length <> 0 then
                    // Compute deltas for triangle partial derivatives of normal
                    let du1 = uvs.[0].X - uvs.[2].X
                    let du2 = uvs.[1].X - uvs.[2].X
                    let dv1 = uvs.[0].Y - uvs.[2].Y
                    let dv2 = uvs.[1].Y - uvs.[2].Y
                    let dn1 = mesh.Normals.[vertexIndices.[0]] - mesh.Normals.[vertexIndices.[2]]
                    let dn2 = mesh.Normals.[vertexIndices.[1]] - mesh.Normals.[vertexIndices.[2]]
                    let determinant = du1 * dv2 - dv1 * du2
                    if determinant = 0.0f then
                        Normal.Zero, Normal.Zero
                    else
                        let invdet = 1.0f / determinant
                        let dndu = ( dv2 * dn1 - dv1 * dn2) * invdet
                        let dndv = (-du2 * dn1 + du1 * dn2) * invdet
                        dndu, dndv
                else
                    Normal.Zero, Normal.Zero

            let dgShading = DifferentialGeometry(dg.Point, ss, ts,
                                                 objectToWorld.Transform(dndu),
                                                 objectToWorld.Transform(dndv),
                                                 dg.U, dg.V, dg.Shape)
            dgShading.DuDx <- dg.DuDx
            dgShading.DvDx <- dg.DvDx
            dgShading.DuDy <- dg.DuDy
            dgShading.DvDy <- dg.DvDy
            dgShading.DpDx <- dg.DpDx
            dgShading.DpDy <- dg.DpDy
            dgShading

    override this.Sample(u1, u2) =
        let b1, b2 = Aether.MonteCarlo.uniformSampleTriangle u1 u2
        let p = b1 * p1 + b2 * p2 + (1.0f - b1 - b2) * p3
        let n = Vector.Cross(p2-p1, p3-p1).ToNormal()
        let nsTemp = Normal.Normalize(n)
        let ns = if reverseOrientation then nsTemp * -1.0f else nsTemp
        p, ns