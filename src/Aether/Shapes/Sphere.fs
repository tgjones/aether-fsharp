namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


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
            let dndu = (f * F - e * G) * invEGF2 * dpdu + (e * F - f * E) * invEGF2 * dpdv
            let dndv = (g * F - f * G) * invEGF2 * dpdu + (f * F - g * E) * invEGF2 * dpdv

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