﻿namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


type DifferentialGeometry(point, dpdu : Vector, dpdv : Vector,
                          dndu, dndv, u, v, shape : Shape) =
    let normal = Vector.Cross(dpdu, dpdv) |> Vector.Normalize |> Vector.ToNormal
    let normal' = if shape.ReverseOrientation <> shape.TransformSwapsHandedness then normal * -1.0f else normal

    member this.Point = point
    member this.Normal = normal'
    member this.DpDu = dpdu

    member this.ComputeDifferentials ray =
        () // TODO


and [<AbstractClass>] Shape(objectToWorld : Transform, reverseOrientation) =

    let worldToObject = Transform.Inverse objectToWorld
    let transformSwapsHandedness = objectToWorld.SwapsHandedness()

    abstract ObjectSpaceBounds: BBox

    abstract WorldSpaceBounds: BBox
    default this.WorldSpaceBounds = 
        objectToWorld |>> this.ObjectSpaceBounds

    member this.ObjectToWorld = objectToWorld
    member this.WorldToObject = worldToObject

    abstract GetShadingGeometry : Transform -> DifferentialGeometry -> DifferentialGeometry
    default this.GetShadingGeometry obj2World dg = dg

    member this.TransformSwapsHandedness = transformSwapsHandedness
    member this.ReverseOrientation = reverseOrientation


and [<AbstractClass>] IntersectableShape(objectToWorld, reverseOrientation) =
    inherit Shape(objectToWorld, reverseOrientation)

    abstract TryIntersect : RaySegment -> (bool * single * single * option<DifferentialGeometry>)

    abstract Intersects : RaySegment -> bool
    default this.Intersects ray =
        let result, _, _, _ = this.TryIntersect(ray)
        result


and [<AbstractClass>] RefinableShape(objectToWorld, reverseOrientation) =
    inherit Shape(objectToWorld, reverseOrientation)

    abstract Refine : unit -> Shape list


type Plane(objectToWorld, reverseOrientation, point, normal) =
    inherit IntersectableShape(objectToWorld, reverseOrientation)

    member this.Point = point
    member this.Normal = normal

    override this.ObjectSpaceBounds =
        raise (System.NotImplementedException())

    override this.TryIntersect ray =
        raise (System.NotImplementedException())


type Sphere(objectToWorld, reverseOrientation, radius) =
    inherit IntersectableShape(objectToWorld, reverseOrientation)

    let phiMax = pi * 2.0f
    let thetaMin = pi
    let thetaMax = pi * 2.0f

    member this.Radius = radius

    override this.ObjectSpaceBounds =
        BBox.FromPoints(Point(-radius, -radius, -radius),
                        Point(radius, radius, radius))

    override this.TryIntersect ray =
        // Initialize output.
        let defaultOutput = (false, nanf, nanf, None)

        // Transform ray to object space.
        let transformedRay = this.WorldToObject |>> ray

        // Compute quadratic sphre coefficients.
        let origin = transformedRay.Origin |> Point.ToVector
        let a = transformedRay.Direction.LengthSquared()
        let b = 2.0f * Vector.Dot(origin, transformedRay.Direction)
        let c = Vector.Dot(origin, origin) - (radius * radius)

        // Solve quadratic equation
        let mutable t0 = 0.0f
        let mutable t1 = 0.0f
        match quadratic a b c with
        | (Some(t0), Some(t1)) ->
            // Compute intersection distance along ray.
            if t0 > transformedRay.MaxT || t1 < transformedRay.MinT then
                defaultOutput
            else
                let intersect tHitTemp =
                    // Compute sphere hit position and phi
                    let pHit = transformedRay.Evaluate tHitTemp
                    let mutable phi = atan2 pHit.Y pHit.X
                    if phi < 0.0f then
                        phi <- phi + pi * 2.0f

                    // Find parametric representation of sphere hit.
                    let u = phi / phiMax
                    let theta = acos (clamp (pHit.Z / radius) -1.0f 1.0f)
                    let v = (theta - thetaMin) / (thetaMax - thetaMin)

                    // Compute sphere dpdu and dpdv.
                    let mutable cosPhi = 0.0f
                    let mutable sinPhi = 0.0f
                    let mutable dpDu = Vector.Zero
                    let mutable dpDv = Vector.Zero
                    let zRadius = sqrt (pHit.X * pHit.X + pHit.Y * pHit.Y)
                    if zRadius = 0.0f then
                        // Handle hit at degenerate parameterization point.
                        cosPhi <- 0.0f
                        sinPhi <- 1.0f
                        dpDv <- (thetaMax - thetaMin) * Vector(pHit.Z * cosPhi, pHit.Z * sinPhi, -radius * (sin theta))
                        dpDu <- Vector.Cross(dpDv, pHit.ToVector())
                    else
                        let inverseZRadius = 1.0f / zRadius
                        cosPhi <- pHit.X * inverseZRadius
                        sinPhi <- pHit.Y * inverseZRadius
                        dpDu <- Vector(-phiMax * pHit.Y, phiMax * pHit.X, 0.0f)
                        // TODO: The following line is identical to the branch above.
                        dpDv <- (thetaMax - thetaMin) * new Vector(pHit.Z * cosPhi, pHit.Z * sinPhi, -radius * (sin theta))

                    // Compute sphere dndu and dndv.
                    let d2Pduu = -phiMax * phiMax * Vector(pHit.X, pHit.Y, 0.0f)
                    let d2Pduv = (thetaMax - thetaMin) * pHit.Z * phiMax * Vector(-sinPhi, cosPhi, 0.0f)
                    let d2Pdvv = -(thetaMax - thetaMin) * (thetaMax - thetaMin) * pHit.ToVector()

                    // Compute coefficients for fundamental forms.
                    let E = Vector.Dot(dpDu, dpDu)
                    let F = Vector.Dot(dpDu, dpDv)
                    let G = Vector.Dot(dpDv, dpDv)
                    let n = Vector.Cross(dpDu, dpDv) |> Vector.Normalize
                    let e = Vector.Dot(n, d2Pduu)
                    let f = Vector.Dot(n, d2Pduv)
                    let g = Vector.Dot(n, d2Pdvv)

                    // Compute dndu and dndv from fundamental form coefficients.
                    let invEGF2 = 1.0f / (E * G - F * F)
                    let dnDu = (f * F - e * G) * invEGF2 * dpDu + (e * F - f * E) * invEGF2 * dpDv
                    let dnDv = (g * F - f * G) * invEGF2 * dpDu + (f * F - g * E) * invEGF2 * dpDv

                    // Initialize differenterial geometry from parametric information.
                    let dg = DifferentialGeometry(this.ObjectToWorld |>> pHit, 
                                                  this.ObjectToWorld |>> dpDu,
                                                  this.ObjectToWorld |>> dpDv, 
                                                  this.ObjectToWorld |>> dnDu, 
                                                  this.ObjectToWorld |>> dnDv,
                                                  u, v, this)

                    let tHit = tHitTemp
                    let rayEpsilon = 5e-4f * tHit
                    (true, tHit, rayEpsilon, Some(dg))

                let mutable tHitTemp = t0
                if t0 < transformedRay.MinT then // Is first intersection before the range we're interested in?
                    tHitTemp <- t1
                    if tHitTemp > transformedRay.MaxT then // Is second intersection after the range we're interested in?
                        defaultOutput
                    else
                        intersect tHitTemp
                else
                    intersect tHitTemp
        | _ -> defaultOutput


type TextureCoordinate = { X : single; Y : single }


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
        // TODO: This is duplicated in other shapes.
        let defaultOutput = (false, nanf, nanf, None)

        let e1 = p2 - p1
        let e2 = p3 - p1
        let s1 = Vector.Cross(ray.Direction, e1)
        let divisor = Vector.Dot(s1, e1)

        if divisor = 0.0f then defaultOutput else
            let invDivisor = 1.0f / divisor

            // Compute first barycentric coordinate
            let d = ray.Origin - p1
            let b1 = Vector.Dot(d, s1) * invDivisor
            if b1 < 0.0f || b1 > 1.0f then defaultOutput else
                
                // Compute second barycentric coordinate
                let s2 = Vector.Cross(d, e1)
                let b2 = Vector.Dot(ray.Direction, s2) * invDivisor
                if b2 < 0.0f || b1 + b2 > 1.0f then defaultOutput else

                    // Compute t to intersection point
                    let t = Vector.Dot(e2, s2) * invDivisor
                    if t < ray.MinT || t > ray.MaxT then defaultOutput else
                        
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
                        (true, t, rayEpsilon, Some(dg))