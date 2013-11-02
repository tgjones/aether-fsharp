namespace Aether.Reflection

open System
open System.Collections.Generic
open Aether
open Aether.Math
open Aether.Geometry
open Aether.Shapes


type Bsdf(dg : DifferentialGeometry, geometricNormal) =
    let bxdfs = List<Bxdf>()
    let nn = dg.Normal
    let sn = dg.DpDu |> Vector.Normalize
    let tn = Normal.Cross(nn, sn)

    member this.DifferentialGeometry = dg

    // TODO: Allow shape to use a shading geometry.
    member this.ShadingGeometry = dg

    member this.Add bxdf =
        bxdfs.Add(bxdf)

    member this.Evaluate(outgoingWorld, incomingWorld, ?flags0) =
        let flags = defaultArg flags0 BxdfType.All

        let worldToLocal (v : Vector) =
            Vector(Vector.Dot(v, sn), 
                   Vector.Dot(v, tn), 
                   Vector.Dot(v, nn))

        let outgoing = worldToLocal(outgoingWorld)
        let incoming = worldToLocal(incomingWorld)

        let mutable result = Spectrum.Black()
        for bxdf in bxdfs do
            result <- result + (bxdf.Evaluate incoming outgoing)
        result