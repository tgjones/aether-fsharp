namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


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


