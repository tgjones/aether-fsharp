namespace Aether.Reflection

open System
open System.Collections.Generic
open Aether
open Aether.Math
open Aether.Geometry


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


[<AutoOpen>]
module private BxdfUtilities =
    let inline cosTheta (w : Vector)    = w.Z
    let inline absCosTheta (w : Vector) = abs w.Z
    let inline sinTheta2 (w : Vector)   = max 0.0f (1.0f - (cosTheta w)*(cosTheta w))
    let inline sinTheta (w : Vector)    = sqrt (sinTheta2(w))
    let inline cosPhi (w : Vector) =
        let sintheta = sinTheta w
        if sintheta = 0.0f then 1.0f
        else clamp (w.X / sintheta) -1.0f 1.0f
    let inline sinPhi (w : Vector) =
        let sintheta = sinTheta w
        if sintheta = 0.0f then 0.0f
        else clamp (w.Y / sintheta) -1.0f 1.0f
    let inline sameHemisphere (w : Vector) (wp : Vector) = w.Z * wp.Z > 0.0f



/// Base class for Brdf (Bi-directional Reflectance Distribution Function)
/// and Btdf (Bi-directional Transmittance Distribution Function)
[<AbstractClass>]
type Bxdf(bxdfType) =
    abstract Evaluate : Vector -> Vector -> Spectrum


type SpecularReflection(reflectance : Spectrum, fresnel : Fresnel) =
    inherit Bxdf(BxdfType.Reflection ||| BxdfType.Specular)

    override this.Evaluate incoming outgoing = Spectrum(0.0f)


type SpecularTransmission(t : Spectrum, etai, etat) =
    inherit Bxdf(BxdfType.Transmission ||| BxdfType.Specular)

    let fresnel = FresnelDielectric(etai, etat)

    override this.Evaluate incoming outgoing = Spectrum(0.0f)


type Lambertian(reflectance : Spectrum) =
    inherit Bxdf(BxdfType.Reflection ||| BxdfType.Diffuse)

    override this.Evaluate incoming outgoing =
        reflectance * invPi


type OrenNayar(reflectance : Spectrum, sigma) =
    inherit Bxdf(BxdfType.Reflection ||| BxdfType.Diffuse)

    let sigmaRadians = toRadians sigma
    let sigma2 = sigma * sigma
    let a = 1.0f - (sigma2 / (2.0f * (sigma2 + 0.33f)))
    let b = 0.45f * sigma2 / (sigma2 + 0.09f)

    override this.Evaluate incoming outgoing =
        let sinthetai = sinTheta incoming
        let sinthetao = sinTheta outgoing

        // Compute cosine term of Oren-Nayar model.
        let maxcos =
            if sinthetai > 1e-4f && sinthetao > 1e-4f then
                let sinphii, cosphii = sinPhi incoming, cosPhi incoming
                let sinphio, cosphio = sinPhi outgoing, cosPhi outgoing
                let dcos = cosphii * cosphio + sinphii * sinphio
                max 0.0f dcos
            else
                0.0f

        // Compute sin and tangent terms of Oren-Nayar model.
        let sinalpha, tanbeta =
            if absCosTheta incoming > absCosTheta outgoing then
                sinthetao, (sinthetai / (absCosTheta incoming))
            else
                sinthetai, (sinthetao / (absCosTheta outgoing))

        reflectance * invPi * (a + b * maxcos * sinalpha * tanbeta)


[<AbstractClass>]
type MicrofacetDistribution() =
    abstract D : Vector -> single