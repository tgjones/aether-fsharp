namespace Aether.Math

open Nexus
open Nexus.Objects3D


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

    type Nexus.Graphics.Colors.ColorF with
        member this.IsEmpty () =
            this.A = 1.0f && this.R = 0.0f && this.G = 0.0f && this.B = 0.0f