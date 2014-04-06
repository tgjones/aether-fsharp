namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


type Disk(objectToWorld : Transform, reverseOrientation,
          height, radius, innerRadius, phiMax') =
    inherit Shape(objectToWorld, reverseOrientation)

    let phiMax = toRadians (clamp phiMax' 0.0f 360.0f)

    let doIntersect (ray' : RaySegment) worldToObject =
        // Transform ray to object space.
        let ray = worldToObject |>> ray'

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
                else Some(thit, dist2, phit, phi)

        match computePlaneIntersection() with
        | Some(tHit) -> computeHitPositionAndPhi tHit
        | None -> None

    let objectSpaceBounds = BBox(Point(-radius, -radius, height),
                                 Point( radius,  radius, height))

    override this.ObjectSpaceBounds = objectSpaceBounds

    override this.TryIntersect(ray) =
        match doIntersect ray this.WorldToObject with
        | Some(tHit, dist2, pHit, phi) ->
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

    override this.Intersects(ray) =
        match doIntersect ray this.WorldToObject with
        | Some(_, _, _, _) -> true
        | None -> false

    override this.Area() =
        phiMax * 0.5f * (radius * radius - innerRadius * innerRadius)

    override this.Sample(u1, u2) =
        let x, y = Aether.MonteCarlo.concentricSampleDisk u1 u2
        let p = Point(x * radius, y * radius, height)
        let ns = objectToWorld.Transform(Normal(0.0f, 0.0f, 1.0f)) |> Normal.Normalize
        let ns' = if reverseOrientation then ns * -1.0f else ns
        (objectToWorld.Transform(p), ns)