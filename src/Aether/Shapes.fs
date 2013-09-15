namespace Aether.Shapes

open Nexus
open Nexus.Graphics.Transforms
open Nexus.Objects3D
open Aether.Math


type DifferentialGeometry(point, dpDu : Vector3D, dpDv : Vector3D, dnDu, dnDv, u, v, shape : Shape) =
    let normal = Normal3D.op_Explicit(Vector3D.Normalize(Vector3D.Cross(dpDu, dpDv)))

    member this.Point = point
    member this.Normal = normal
    member this.DpDu = dpDu

    member this.ComputeDifferentials ray =
        () // TODO


and [<AbstractClass>] Shape(objectToWorld : Transform3D) =
    let worldToObject = objectToWorld.Inverse

    abstract ObjectSpaceBounds: AxisAlignedBox3D

    abstract WorldSpaceBounds: AxisAlignedBox3D
    default this.WorldSpaceBounds = 
        objectToWorld.Transform(this.ObjectSpaceBounds)

    member this.ObjectToWorld = objectToWorld
    member this.WorldToObject = worldToObject


and [<AbstractClass>] IntersectableShape(objectToWorld) =
    inherit Shape(objectToWorld)

    abstract TryIntersect : RaySegment3D -> (bool * single * option<DifferentialGeometry>)

    abstract Intersects : RaySegment3D -> bool
    default this.Intersects ray =
        let result, tHit, dg = this.TryIntersect(ray)
        result


and [<AbstractClass>] RefinableShape(objectToWorld) =
    inherit Shape(objectToWorld)

    abstract Refine : unit -> Shape list


type Plane(objectToWorld, point, normal) =
    inherit IntersectableShape(objectToWorld)

    member this.Point = point
    member this.Normal = normal

    override this.ObjectSpaceBounds =
        raise (System.NotImplementedException())

    override this.TryIntersect ray =
        raise (System.NotImplementedException())


type Sphere(objectToWorld, radius) =
    inherit IntersectableShape(objectToWorld)

    let phiMax = MathUtility.TWO_PI
    let thetaMin = MathUtility.PI
    let thetaMax = MathUtility.TWO_PI

    member this.Radius = radius

    override this.ObjectSpaceBounds =
        AxisAlignedBox3D(Point3D(-radius, -radius, -radius),
                         Point3D(radius, radius, radius))

    override this.TryIntersect ray =
        // Initialize output.
        let defaultOutput = (false, System.Single.MinValue, None)

        // Transform ray to object space.
        let transformedRay = this.WorldToObject.Transform(ray)

        // Compute quadratic sphre coefficients.
        let origin = Point3D.op_Explicit(transformedRay.Origin)
        let a = transformedRay.Direction.LengthSquared()
        let b = 2.0f * Vector3D.Dot(origin, transformedRay.Direction)
        let c = Vector3D.Dot(origin, origin) - (radius * radius)

        // Solve quadratic equation
        let mutable t0 = 0.0f
        let mutable t1 = 0.0f
        if not(MathUtility.Quadratic(a, b, c, &t0, &t1)) then
            defaultOutput
        else
            // Compute intersection distance along ray.
            if t0 > transformedRay.MaxT || t1 < transformedRay.MinT then
                defaultOutput
            else
                let intersect tHitTemp =
                    // Compute sphere hit position and phi
                    let pHit = transformedRay.Evaluate(tHitTemp)
                    let mutable phi = MathUtility.Atan2(pHit.Y, pHit.X)
                    if phi < 0.0f then
                        phi <- phi + MathUtility.TWO_PI

                    // Find parametric representation of sphere hit.
                    let u = phi / phiMax
                    let theta = MathUtility.Acos(MathUtility.Clamp(pHit.Z / radius, -1.0f, 1.0f))
                    let v = (theta - thetaMin) / (thetaMax - thetaMin)

                    // Compute sphere dpdu and dpdv.
                    let mutable cosPhi = 0.0f
                    let mutable sinPhi = 0.0f
                    let mutable dpDu = Vector3D()
                    let mutable dpDv = Vector3D()
                    let zRadius = MathUtility.Sqrt(pHit.X * pHit.X + pHit.Y * pHit.Y)
                    if zRadius = 0.0f then
                        // Handle hit at degenerate parameterization point.
                        cosPhi <- 0.0f
                        sinPhi <- 1.0f
                        dpDv <- (thetaMax - thetaMin) * Vector3D(pHit.Z * cosPhi, pHit.Z * sinPhi, -radius * MathUtility.Sin(theta))
                        dpDu <- Vector3D.Cross(dpDv, Point3D.op_Explicit(pHit))
                    else
                        let inverseZRadius = 1.0f / zRadius
                        cosPhi <- pHit.X * inverseZRadius
                        sinPhi <- pHit.Y * inverseZRadius
                        dpDu <- Vector3D(-phiMax * pHit.Y, phiMax * pHit.X, 0.0f)
                        // TODO: The following line is identical to the branch above.
                        dpDv <- (thetaMax - thetaMin) * new Vector3D(pHit.Z * cosPhi, pHit.Z * sinPhi, -radius * MathUtility.Sin(theta))

                    // Compute sphere dndu and dndv.
                    let d2Pduu = -phiMax * phiMax * Vector3D(pHit.X, pHit.Y, 0.0f)
                    let d2Pduv = (thetaMax - thetaMin) * pHit.Z * phiMax * Vector3D(-sinPhi, cosPhi, 0.0f)
                    let d2Pdvv = -(thetaMax - thetaMin) * (thetaMax - thetaMin) * Point3D.op_Explicit(pHit)

                    // Compute coefficients for fundamental forms.
                    let E = dpDu.LengthSquared()
                    let F = Vector3D.Dot(dpDu, dpDv)
                    let G = dpDv.LengthSquared()
                    let n = Vector3D.Normalize(Vector3D.Cross(dpDu, dpDv))
                    let e = Vector3D.Dot(n, d2Pduu)
                    let f = Vector3D.Dot(n, d2Pduv)
                    let g = Vector3D.Dot(n, d2Pdvv)

                    // Compute dndu and dndv from fundamental form coefficients.
                    let invEGF2 = 1.0f / (E * G - F * F)
                    let dnDu = (f * F - e * G) * invEGF2 * dpDu + (e * F - f * E) * invEGF2 * dpDv
                    let dnDv = (g * F - f * G) * invEGF2 * dpDu + (f * F - g * E) * invEGF2 * dpDv

                    // Initialize differenterial geometry from parametric information.
                    let dg = Some(DifferentialGeometry(this.ObjectToWorld.Transform(pHit), 
                                                   this.ObjectToWorld.Transform(dpDu),
                                                   this.ObjectToWorld.Transform(dpDv), 
                                                   this.ObjectToWorld.Transform(dnDu), 
                                                   this.ObjectToWorld.Transform(dnDv),
                                                   u, v, this))

                    let tHit = tHitTemp
                    (true, tHit, dg)

                let mutable tHitTemp = t0
                if t0 < transformedRay.MinT then // Is first intersection before the range we're interested in?
                    tHitTemp <- t1
                    if tHitTemp > transformedRay.MaxT then // Is second intersection after the range we're interested in?
                        defaultOutput
                    else
                        intersect tHitTemp
                else
                    intersect tHitTemp


type TriangleMesh(objectToWorld, numTriangles, numVertices, 
                  vertexIndices : int[],
                  points : Point3D list,
                  normals : Normal3D list,
                  s,
                  textureCoordinates : Point2D list option) =
    inherit RefinableShape(objectToWorld)

    // Transform mesh vertices to world space.
    let worldSpacePoints = points |> List.map objectToWorld.Transform

    member this.VertexIndices = vertexIndices
    member this.Points = points
    member this.TextureCoordinates = textureCoordinates

    override this.ObjectSpaceBounds = AxisAlignedBox3D(points)
    override this.WorldSpaceBounds = AxisAlignedBox3D(worldSpacePoints)

    override this.Refine () =
        [ for n in [0 .. numTriangles-1] ->
              Triangle(objectToWorld, this, n) :> Shape ]


and Triangle(objectToWorld, mesh : TriangleMesh, n) =
    inherit IntersectableShape(objectToWorld)

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
            [ Point2D(0.0f, 0.0f)
              Point2D(1.0f, 0.0f)
              Point2D(1.0f, 1.0f) ]

    override this.ObjectSpaceBounds =
        AxisAlignedBox3D([ this.WorldToObject.Transform(p1)
                           this.WorldToObject.Transform(p2)
                           this.WorldToObject.Transform(p3) ])

    override this.WorldSpaceBounds =
        AxisAlignedBox3D([ p1; p2; p3 ])

    override this.TryIntersect ray =
        // TODO: This is duplicated in other shapes.
        let defaultOutput = (false, System.Single.MinValue, None)

        let e1 = p2 - p1
        let e2 = p3 - p1
        let s1 = Vector3D.Cross(ray.Direction, e1)
        let divisor = Vector3D.Dot(s1, e1)

        if divisor = 0.0f then defaultOutput else
            let invDivisor = 1.0f / divisor

            // Compute first barycentric coordinate
            let d = ray.Origin - p1
            let b1 = Vector3D.Dot(d, s1) * invDivisor
            if b1 < 0.0f || b1 > 1.0f then defaultOutput else
                
                // Compute second barycentric coordinate
                let s2 = Vector3D.Cross(d, e1)
                let b2 = Vector3D.Dot(ray.Direction, s2) * invDivisor
                if b2 < 0.0f || b1 + b2 > 1.0f then defaultOutput else

                    // Compute t to intersection point
                    let t = Vector3D.Dot(e2, s2) * invDivisor
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
                        let dg = DifferentialGeometry(ray.Evaluate(t), dpdu, dpdv,
                                                      Vector3D.Zero, Vector3D.Zero,
                                                      tu, tv, this)
                        (true, t, Some(dg))