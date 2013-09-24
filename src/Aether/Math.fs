namespace Aether.Math

open Nexus
open Nexus.Objects3D


[<AutoOpen>]
module GlobalFunctions =

    let pi = single(System.Math.PI)
    let invPi = 1.0f / pi
    let invTwoPi = 1.0f / (2.0f * pi)
    let invFourPi = 1.0f / (4.0f * pi)

    let inline lerp t v1 v2 = 
        (1.0f - t) * v1 + (t * v2)

    let inline clamp value low high =
        if value < low then low
        else if value > high then high
        else value

    let inline toRadians degrees =
        pi / 180.0f * degrees

    let inline toDegrees radians =
        180.0f / pi * radians

    let invLog2 = 1.0f / (log 2.0f)

    let inline log2 x =
        (log x) * invLog2

    let inline floor2Int value =
        int(floor value)

    let inline log2int value =
        floor2Int (log2 value)
        
    let inline isPowerOf2 value =
        value &&& (value - 1) = 0

    let inline swap (left : 'a byref) (right : 'a byref) =
        let temp = left
        left <- right
        right <- temp

    let inline quadratic a b c =
        // Find quadratic discriminant.
        let discrim = b * b - 4.0f * a * c

        if discrim < 0.0f then (false, None, None)
        else
            let rootDiscrim = sqrt discrim

            // Compute quadratic t values.
            let q = if b < 0.0f then -0.5f * (b - rootDiscrim)
                    else -0.5f * (b + rootDiscrim)
            let mutable t0 = q / a
            let mutable t1 = c / q
            if t0 > t1 then swap &t0 &t1
            (true, Some(t0), Some(t1))


type RaySegment3D(origin : Point3D, direction : Vector3D,
                  ?minT0 : single, ?maxT0 : single,
                  ?time0 : single) =

    let epsilon = RaySegment3D.Epsilon
    let minT = defaultArg minT0 epsilon
    let maxT = defaultArg maxT0 System.Single.MaxValue
    let time = defaultArg time0 0.0f

    member this.Origin = origin
    member this.Direction = direction
    member this.MinT = minT
    member val MaxT = maxT with get, set
    member this.Time = time

    member x.Evaluate(t : single) = origin + direction * t

    static member Epsilon = 1e-3f


[<AutoOpen>]
module Extensions =
    type Nexus.Graphics.Transforms.Transform3D with 
        member this.Transform (ray : RaySegment3D) =
           let transformedRay = this.Transform(Ray3D(ray.Origin, ray.Direction))
           RaySegment3D(transformedRay.Origin, transformedRay.Direction,
                        ray.MinT, ray.MaxT, ray.Time)

    type Nexus.Graphics.Colors.Spectrum with
        member this.ToColorRgbF () =
            let xyzToRgb (xyz : single[]) (rgb : single[]) =
                rgb.[0] <-  3.240479f*xyz.[0] - 1.537150f*xyz.[1] - 0.498535f*xyz.[2]
                rgb.[1] <- -0.969256f*xyz.[0] + 1.875991f*xyz.[1] + 0.041556f*xyz.[2]
                rgb.[2] <-  0.055648f*xyz.[0] - 0.204043f*xyz.[1] + 1.057311f*xyz.[2]

            let xyz = Array.create 3 0.0f
            this.Xyz(xyz)

            let rgb = Array.create 3 0.0f
            xyzToRgb xyz rgb

            Nexus.Graphics.Colors.ColorRgbF(rgb.[0], rgb.[1], rgb.[2])