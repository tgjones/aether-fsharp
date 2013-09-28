namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


type TriangleMesh(objectToWorld : Transform, reverseOrientation, numTriangles, numVertices, 
                  vertexIndices : int[],
                  points : Point list,
                  normals : Normal list,
                  s,
                  textureCoordinates : TextureCoordinate list option) =
    inherit RefinableShape(objectToWorld, reverseOrientation)

    // Transform mesh vertices to world space.
    let worldSpacePoints = points |> List.map (fun x -> objectToWorld |>> x)

    let objectSpaceBounds = BBox.FromPoints(points)
    let worldSpaceBounds = BBox.FromPoints(worldSpacePoints)

    member this.VertexIndices = vertexIndices
    member this.Points = points
    member this.TextureCoordinates = textureCoordinates

    override this.ObjectSpaceBounds = objectSpaceBounds
    override this.WorldSpaceBounds = worldSpaceBounds

    override this.Refine () =
        [ for n in [0 .. numTriangles-1] ->
              Triangle(objectToWorld, reverseOrientation, this, n) :> Shape ]


and Triangle(objectToWorld, reverseOrientation, mesh : TriangleMesh, n) =
    inherit IntersectableShape(objectToWorld, reverseOrientation)

    let vertexIndices = mesh.VertexIndices.[n..n+2]

    // Get triangle vertices in p1, p2, p3
    let p1 = mesh.Points.[vertexIndices.[0]]
    let p2 = mesh.Points.[vertexIndices.[1]]
    let p3 = mesh.Points.[vertexIndices.[2]]

    // Get texture coordinates
    let uvs =
        match mesh.TextureCoordinates with
        | Some(uvs) ->
            [ uvs.[vertexIndices.[0]]
              uvs.[vertexIndices.[1]]
              uvs.[vertexIndices.[2]] ]
        | None ->
            [ { X = 0.0f; Y = 0.0f }
              { X = 1.0f; Y = 0.0f }
              { X = 0.0f; Y = 1.0f } ]

    override this.ObjectSpaceBounds =
        BBox.FromPoints [ this.WorldToObject |>> p1
                          this.WorldToObject |>> p2
                          this.WorldToObject |>> p3 ]

    override this.WorldSpaceBounds =
        BBox.FromPoints [ p1; p2; p3 ]

    override this.TryIntersect ray =
        let e1 = p2 - p1
        let e2 = p3 - p1
        let s1 = Vector.Cross(ray.Direction, e1)
        let divisor = Vector.Dot(s1, e1)

        if divisor = 0.0f then None else
            let invDivisor = 1.0f / divisor

            // Compute first barycentric coordinate
            let d = ray.Origin - p1
            let b1 = Vector.Dot(d, s1) * invDivisor
            if b1 < 0.0f || b1 > 1.0f then None else
                
                // Compute second barycentric coordinate
                let s2 = Vector.Cross(d, e1)
                let b2 = Vector.Dot(ray.Direction, s2) * invDivisor
                if b2 < 0.0f || b1 + b2 > 1.0f then None else

                    // Compute t to intersection point
                    let t = Vector.Dot(e2, s2) * invDivisor
                    if t < ray.MinT || t > ray.MaxT then None else
                        
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

                        // Test intersection against alpha texture, if present
                        // TODO

                        // Fill in DifferentialGeometry from triangle hit
                        let dg = DifferentialGeometry(ray.Evaluate t, dpdu, dpdv,
                                                      Vector.Zero, Vector.Zero,
                                                      tu, tv, this)
                        let rayEpsilon = 1e-3f * t
                        Some(t, rayEpsilon, dg)