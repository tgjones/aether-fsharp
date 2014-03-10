namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


type DifferentialGeometry(point : Point, dpdu : Vector, dpdv : Vector,
                          dndu, dndv, u : single, v : single,
                          shape : Shape) =
    let normal = Vector.Cross(dpdu, dpdv) |> Vector.Normalize |> Vector.ToNormal
    let normal' = if shape.ReverseOrientation <> shape.TransformSwapsHandedness then normal * -1.0f else normal

    member this.Point = point
    member this.Normal = normal'
    member this.U = u
    member this.V = v
    member this.DpDu = dpdu

    member val DpDx = Vector.Zero with get, set
    member val DpDy = Vector.Zero with get, set
    member val DuDx = 0.0f with get, set
    member val DvDx = 0.0f with get, set
    member val DuDy = 0.0f with get, set
    member val DvDy = 0.0f with get, set

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


[<AbstractClass>]
type IntersectableShape(objectToWorld, reverseOrientation) =
    inherit Shape(objectToWorld, reverseOrientation)

    abstract TryIntersect : RaySegment -> (single * single * DifferentialGeometry) option

    abstract Intersects : RaySegment -> bool
    default this.Intersects ray =
        match this.TryIntersect(ray) with
        | Some(_) -> true
        | None -> false


[<AbstractClass>]
type RefinableShape(objectToWorld, reverseOrientation) =
    inherit Shape(objectToWorld, reverseOrientation)

    abstract Refine : unit -> Shape list


type TextureCoordinate = { X : single; Y : single }


type Disk(objectToWorld : Transform, reverseOrientation,
          height, radius, innerRadius, phiMax') =
    inherit IntersectableShape(objectToWorld, reverseOrientation)

    let phiMax = toRadians (clamp phiMax' 0.0f 360.0f)

    let objectSpaceBounds = BBox(Point(-radius, -radius, height),
                                 Point( radius,  radius, height))

    override this.ObjectSpaceBounds = objectSpaceBounds

    override this.TryIntersect ray' =
        // Transform ray to object space.
        let ray = this.WorldToObject |>> ray'

        let computePlaneIntersection() =
            if (abs ray.Direction.Z) < 1e-7f then
                None
            else
                let thit = (height - ray.Origin.Z) / ray.Direction.Z
                if thit < ray.MinT || thit > ray.MaxT then None
                else Some(thit)

        let computeHitPositionAndPhi thit =
            let phit = ray.Evaluate(thit)
            let dist2 = phit.X * phit.X + phit.Y * phit.Y
            if dist2 > radius * radius || dist2 < innerRadius * innerRadius then
                None
            else
                let phiTemp = atan2 phit.Y phit.X
                let phi = if phiTemp < 0.0f then phiTemp + 2.0f * pi
                          else phiTemp
                if (phi > phiMax) then None
                else Some(dist2, phit, phi)

        match computePlaneIntersection() with
        | Some(tHit) ->
            match computeHitPositionAndPhi tHit with
            | Some(dist2, pHit, phi) ->
                // Find parametric representation of disk hit.
                let u = phi / phiMax
                let oneMinusV = (sqrt(dist2) - innerRadius) / (radius - innerRadius)
                let invOneMinusV = if oneMinusV > 0.0f then 1.0f / oneMinusV else 0.0f
                let v = 1.0f - oneMinusV
                let dpdu = Vector(-phiMax * pHit.Y, phiMax * pHit.X, 0.0f)
                           * phiMax * invTwoPi
                let dpdv = Vector(-pHit.X * invOneMinusV, -pHit.Y * invOneMinusV, 0.0f) 
                           * (radius - innerRadius) / radius
                let dndu, dndv = Normal.Zero, Normal.Zero

                // Initialize differenterial geometry from parametric information.
                let dg = DifferentialGeometry(this.ObjectToWorld |>> pHit, 
                                              this.ObjectToWorld |>> dpdu,
                                              this.ObjectToWorld |>> dpdv, 
                                              this.ObjectToWorld |>> dndu, 
                                              this.ObjectToWorld |>> dndv,
                                              u, v, this)

                Some(tHit, 5e-4f * tHit, dg)
            | None -> None
        | None -> None


type Sphere(objectToWorld, reverseOrientation, radius, zMin, zMax, phiMax') =
    inherit IntersectableShape(objectToWorld, reverseOrientation)

    let thetaMin = acos (clamp (zMin / radius) -1.0f 1.0f)
    let thetaMax = acos (clamp (zMax / radius) -1.0f 1.0f)
    let thetaDiff = thetaMax - thetaMin
    let phiMax = toRadians (clamp phiMax' 0.0f 360.0f)

    member this.Radius = radius

    override this.ObjectSpaceBounds =
        BBox.FromPoints(Point(-radius, -radius, -radius),
                        Point(radius, radius, radius))

    override this.TryIntersect ray =
        // Transform ray to object space.
        let transformedRay = this.WorldToObject |>> ray

        // Compute quadratic sphere coefficients.
        let origin = transformedRay.Origin |> Point.ToVector
        let a = transformedRay.Direction.LengthSquared()
        let b = 2.0f * Vector.Dot(origin, transformedRay.Direction)
        let c = Vector.Dot(origin, origin) - (radius * radius)

        let findFirstIntersectionInValidRange t0 t1 =
            if t0 > transformedRay.MaxT || t1 < transformedRay.MinT then
                None
            else if t0 < transformedRay.MinT then 
                if t1 > transformedRay.MaxT then
                    None
                else
                    Some(t1)
            else
                Some(t0)

        let computeSphereHitPositionAndPhi tHit =
            let pHitTemp = transformedRay.Evaluate tHit
            let pHit = if pHitTemp.X = 0.0f && pHitTemp.Y = 0.0f then
                            Point(1e-5f * radius, pHitTemp.Y, pHitTemp.Z)
                        else pHitTemp
            let phiTemp = atan2 pHit.Y pHit.X
            let phi = if phiTemp < 0.0f then phiTemp + 2.0f * pi
                        else phiTemp
            (pHit, phi)

        let testSphereIntersectionAgainstClippingParameters (pHit : Point) phi =
            (zMin > -radius && pHit.Z < zMin) || 
            (zMax < radius && pHit.Z > zMax) || 
            (phi > phiMax)

        // Finds the first sphere hit that matches clipping parameters.
        let findValidSphereHit tHit t1 =
            // Try first hit - which may be t0 or t1.
            let (pHit, phi) = computeSphereHitPositionAndPhi tHit

            // Test it against clipping parameters.
            if (testSphereIntersectionAgainstClippingParameters pHit phi) then
                if tHit = t1 then
                    None
                else if t1 > transformedRay.MaxT then
                    None
                else
                    // Try again with t1.
                    let (pHit', phi') = computeSphereHitPositionAndPhi t1
                    if (testSphereIntersectionAgainstClippingParameters pHit' phi') then
                        None
                    else
                        Some(t1, pHit', phi')
            else
                Some(tHit, pHit, phi)

        let getSphereHit t0 t1 =
            match findFirstIntersectionInValidRange t0 t1 with
            | Some(tHit) -> findValidSphereHit tHit t1
            | None -> None

        let findParametricRepresentation (pHit : Point) phi =
            let u = phi / phiMax
            let theta = acos (clamp (pHit.Z / radius) -1.0f 1.0f)
            let v = (theta - thetaMin) / thetaDiff
            (u, v, theta)

        let computeDifferentials (pHit : Point) theta =
            // Compute sphere dpdu and dpdv.
            let zRadius = sqrt (pHit.X * pHit.X + pHit.Y * pHit.Y)
            let invZRadius = 1.0f / zRadius
            let cosPhi = pHit.X * invZRadius
            let sinPhi = pHit.Y * invZRadius
            let dpdu = Vector(-phiMax * pHit.Y, phiMax * pHit.X, 0.0f)
            let dpdv = (thetaDiff) * Vector(pHit.Z * cosPhi,
                                            pHit.Z * sinPhi,
                                            -radius * sin theta)

            // Compute sphere dndu and dndv.
            let d2Pduu = -phiMax * phiMax * Vector(pHit.X, pHit.Y, 0.0f)
            let d2Pduv = thetaDiff * pHit.Z * phiMax * Vector(-sinPhi, cosPhi, 0.0f)
            let d2Pdvv = -thetaDiff * thetaDiff * pHit.ToVector()

            // Compute coefficients for fundamental forms.
            let E = Vector.Dot(dpdu, dpdu)
            let F = Vector.Dot(dpdu, dpdv)
            let G = Vector.Dot(dpdv, dpdv)
            let n = Vector.Cross(dpdu, dpdv) |> Vector.Normalize
            let e = Vector.Dot(n, d2Pduu)
            let f = Vector.Dot(n, d2Pduv)
            let g = Vector.Dot(n, d2Pdvv)

            // Compute dndu and dndv from fundamental form coefficients.
            let invEGF2 = 1.0f / (E*G - F*F)
            let dndu = ((f * F - e * G) * invEGF2 * dpdu + (e * F - f * E) * invEGF2 * dpdv).ToNormal()
            let dndv = ((g * F - f * G) * invEGF2 * dpdu + (f * F - g * E) * invEGF2 * dpdv).ToNormal()

            (dpdu, dpdv, dndu, dndv)

        // Solve quadratic equation
        match quadratic a b c with
        | (Some(t0), Some(t1)) ->
            match getSphereHit t0 t1 with
            | Some(tHit, pHit, phi) ->
                let u, v, theta = findParametricRepresentation pHit phi
                let dpdu, dpdv, dndu, dndv = computeDifferentials pHit theta

                // Initialize differenterial geometry from parametric information.
                let dg = DifferentialGeometry(this.ObjectToWorld |>> pHit, 
                                              this.ObjectToWorld |>> dpdu,
                                              this.ObjectToWorld |>> dpdv, 
                                              this.ObjectToWorld |>> dndu, 
                                              this.ObjectToWorld |>> dndv,
                                              u, v, this)

                Some(tHit, 5e-4f * tHit, dg)
            | _ -> None
        | _ -> None


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
                                                      Normal.Zero, Normal.Zero,
                                                      tu, tv, this)
                        let rayEpsilon = 1e-3f * t
                        Some(t, rayEpsilon, dg)