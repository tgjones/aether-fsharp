namespace Aether.Shapes

open Nexus
open Nexus.Graphics.Transforms
open Nexus.Objects3D
open Aether.Math


type DifferentialGeometry(point, dpDu : Vector3D, dpDv : Vector3D, dnDu, dnDv, u, v, shape) =
    let normal = Normal3D.op_Explicit(Vector3D.Normalize(Vector3D.Cross(dpDu, dpDv)))

    member this.Point = point
    member this.Normal = normal
    member this.DpDu = dpDu

    member this.ComputeDifferentials ray =
        () // TODO


[<AbstractClass>]
type Shape(objectToWorld : Transform3D) =
    let worldToObject = objectToWorld.Inverse

    abstract ObjectSpaceBounds: AxisAlignedBox3D

    abstract WorldSpaceBounds: AxisAlignedBox3D
    default this.WorldSpaceBounds = 
        objectToWorld.Transform(this.ObjectSpaceBounds)

    member this.ObjectToWorld = objectToWorld
    member this.WorldToObject = worldToObject

    abstract TryIntersect : RaySegment3D -> (bool * single * option<DifferentialGeometry>)

    abstract Intersects : RaySegment3D -> bool
    default this.Intersects ray =
        let result, tHit, dg = this.TryIntersect(ray)
        result


type Plane(objectToWorld, point, normal) =
    inherit Shape(objectToWorld)

    member this.Point = point
    member this.Normal = normal

    override this.ObjectSpaceBounds =
        raise (System.NotImplementedException())

    override this.TryIntersect ray =
        raise (System.NotImplementedException())


type Sphere(objectToWorld, radius) =
    inherit Shape(objectToWorld)

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