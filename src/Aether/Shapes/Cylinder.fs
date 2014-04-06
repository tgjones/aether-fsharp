namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


type Cylinder(objectToWorld, reverseOrientation, radius, zMin, zMax, phiMax') =
    inherit Shape(objectToWorld, reverseOrientation)

    let phiMax = toRadians (clamp phiMax' 0.0f 360.0f)

    let doIntersect (ray' : RaySegment) worldToObject =
        // Transform ray to object space.
        let ray = worldToObject |>> ray'

        // Compute quadratic cylinder coefficients.
        let a = ray.Direction.X * ray.Direction.X + ray.Direction.Y * ray.Direction.Y
        let b = 2.0f * (ray.Direction.X * ray.Origin.X + ray.Direction.Y * ray.Origin.Y)
        let c = ray.Origin.X * ray.Origin.X + ray.Origin.Y * ray.Origin.Y - radius * radius

        let findFirstIntersectionInValidRange t0 t1 =
            if t0 > ray.MaxT || t1 < ray.MinT then None
            elif t0 < ray.MinT then 
                if t1 > ray.MaxT then None
                else Some(t1)
            else Some(t0)

        let computeHitPositionAndPhi tHit =
            let pHit = ray.Evaluate tHit
            let phiTemp = atan2 pHit.Y pHit.X
            let phi = if phiTemp < 0.0f then phiTemp + 2.0f * pi
                      else phiTemp
            (pHit, phi)

        let testIntersectionAgainstClippingParameters (pHit : Point) phi =
            pHit.Z < zMin || pHit.Z > zMax || phi > phiMax

        // Finds the first hit that matches clipping parameters.
        let findValidHit tHit t1 =
            // Try first hit - which may be t0 or t1.
            let pHit, phi = computeHitPositionAndPhi tHit

            // Test it against clipping parameters.
            if testIntersectionAgainstClippingParameters pHit phi then
                if tHit = t1 then None
                elif t1 > ray.MaxT then None
                else
                    // Try again with t1.
                    let (pHit', phi') = computeHitPositionAndPhi t1
                    if testIntersectionAgainstClippingParameters pHit' phi' then None
                    else Some(t1, pHit', phi')
            else
                Some(tHit, pHit, phi)

        let getHit t0 t1 =
            match findFirstIntersectionInValidRange t0 t1 with
            | Some(tHit) -> findValidHit tHit t1
            | None -> None

        // Solve quadratic equation for t values.
        match quadratic a b c with
        | (Some(t0), Some(t1)) -> getHit t0 t1
        | _ -> None

    override this.ObjectSpaceBounds =
        BBox.FromPoints(Point(-radius, -radius, zMin),
                        Point(radius, radius, zMax))

    override this.TryIntersect ray' =
        let findParametricRepresentation (pHit : Point) phi =
            let u = phi / phiMax
            let v = (pHit.Z - zMin) / (zMax - zMin)
            (u, v)

        let computeDifferentials (pHit : Point) =
            // Compute dpdu and dpdv.
            let dpdu = Vector(-phiMax * pHit.Y, phiMax * pHit.X, 0.0f)
            let dpdv = Vector(0.0f, 0.0f, zMax - zMin)

            // Compute dndu and dndv.
            let d2Pduu = -phiMax * phiMax * Vector(pHit.X, pHit.Y, 0.0f)
            let d2Pduv = Vector.Zero
            let d2Pdvv = Vector.Zero

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

        match doIntersect ray' this.WorldToObject with
        | Some(tHit, pHit, phi) ->
            let u, v = findParametricRepresentation pHit phi
            let dpdu, dpdv, dndu, dndv = computeDifferentials pHit

            // Initialize differential geometry from parametric information.
            let dg = DifferentialGeometry(this.ObjectToWorld |>> pHit, 
                                          this.ObjectToWorld |>> dpdu,
                                          this.ObjectToWorld |>> dpdv, 
                                          this.ObjectToWorld |>> dndu, 
                                          this.ObjectToWorld |>> dndv,
                                          u, v, this)

            Some(tHit, 5e-4f * tHit, dg)
        | _ -> None

    override this.Intersects(ray) =
        match doIntersect ray this.WorldToObject with
        | Some(_, _, _) -> true
        | None -> false

    override this.Area() =
        (zMax - zMin) * phiMax * radius

    override this.Sample(u1, u2) =
        let z = lerp u1 zMin zMax
        let t = u2 * phiMax
        let p = Point(radius * cos t, radius * sin t, z)
        let nsTemp = objectToWorld.Transform(Normal(p.X, p.Y, 0.0f)) |> Normal.Normalize
        let ns = if reverseOrientation then nsTemp * -1.0f else nsTemp
        objectToWorld.Transform(p), ns