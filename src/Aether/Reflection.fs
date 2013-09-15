namespace Aether.Reflection

open System
open System.Collections.Generic
open Nexus
open Nexus.Graphics.Colors
open Aether.Shapes


[<Flags>]
type BxdfType =
    | Reflection      = 0b00001
    | Transmission    = 0b00010

    | Diffuse         = 0b00100
    | Glossy          = 0b01000
    | Specular        = 0b10000

    | AllTypes        = 0b11100 // Diffuse | Glossy | Specular

    | AllReflection   = 0b11101 // Reflection | AllTypes
    | AllTransmission = 0b11110 // Transmission | AllTypes

    | All             = 0b11111 // AllReflection | AllTransmission


/// <summary>
/// Base class for Brdf (Bi-directional Reflectance Distribution Function)
/// and Btdf (Bi-directional Transmittance Distribution Function)
/// </summary>
[<AbstractClass>]
type Bxdf(bxdfType) =
    abstract member Evaluate: Vector3D -> Vector3D -> ColorF


type Lambertian(reflectance : ColorF) =
    inherit Bxdf(BxdfType.Reflection ||| BxdfType.Diffuse)

    override this.Evaluate incoming outgoing =
        reflectance / MathUtility.PI


type OrenNayar(reflectance, sigma) =
    inherit Bxdf(BxdfType.Reflection ||| BxdfType.Diffuse)

    let sigmaRadians = MathUtility.ToRadians(sigma)
    let sigma2 = sigma * sigma
    let a = 1.0f - (sigma2 / (2.0f * (sigma2 + 0.33f)))
    let b = 0.45f * sigma2 / (sigma2 + 0.09f)

    override this.Evaluate incoming outgoing =
        raise (NotImplementedException())


type Bsdf(dg : DifferentialGeometry, geometricNormal) =
    let bxdfs = List<Bxdf>()
    let nn = dg.Normal
    let sn = Vector3D.Normalize(dg.DpDu)
    let tn = Vector3D.Cross(nn, sn)

    member this.DifferentialGeometry = dg

    // TODO: Allow shape to use a shading geometry.
    member this.ShadingGeometry = dg

    member this.Add bxdf =
        bxdfs.Add(bxdf)

    member this.Evaluate(outgoingWorld, incomingWorld, ?flags0) =
        let flags = defaultArg flags0 BxdfType.All

        let worldToLocal (v : Vector3D) =
            Vector3D(Vector3D.Dot(v, sn), 
                     Vector3D.Dot(v, tn), 
                     Vector3D.Dot(v, nn))

        let outgoing = worldToLocal(outgoingWorld)
        let incoming = worldToLocal(incomingWorld)

        let mutable result = ColorF()
        for bxdf in bxdfs do
            result <- result + (bxdf.Evaluate incoming outgoing)
        result