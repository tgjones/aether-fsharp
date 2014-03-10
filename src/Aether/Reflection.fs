namespace Aether.Reflection

open System
open System.Collections.Generic
open Aether
open Aether.Math
open Aether.Geometry
open Aether.Shapes


[<AbstractClass>]
type Fresnel() =
    abstract Evaluate : single -> Spectrum


type FresnelConductor(eta, k) =
    inherit Fresnel()

    let fresnelConductor (cosi : single) (eta : Spectrum) (k : Spectrum) =
        let tmp = (eta*eta + k*k) * cosi * cosi
        let rparl2 = (tmp - (2.0f * eta * cosi) + Spectrum(1.0f)) /
                     (tmp + (2.0f * eta * cosi) + Spectrum(1.0f))
        let tmpf = eta*eta + k*k
        let rperp2 = (tmpf - (2.0f * eta * cosi) + Spectrum(cosi*cosi)) /
                     (tmpf + (2.0f * eta * cosi) + Spectrum(cosi*cosi))
        (rparl2 + rperp2) / 2.0f

    override this.Evaluate(cosi) =
        (fresnelConductor (abs cosi) eta k)


type FresnelDielectric(etai, etat) =
    inherit Fresnel()

    let fresnelDielectric (cosi : single) (cost : single)
                          (etai : Spectrum) (etat : Spectrum) =
        let rparl = ((etat * cosi) - (etai * cost)) /
                    ((etat * cosi) + (etai * cost))
        let rperp = ((etai * cosi) - (etat * cost)) /
                    ((etai * cosi) + (etat * cost))
        (rparl * rparl + rperp * rperp) / 2.0f

    override this.Evaluate(cosi) =
        // Compute Fresnel reflectance for dielectric.
        let cosi' = clamp cosi -1.0f 1.0f

        // Compute indices of refraction for dielectric.
        let entering = cosi > 0.0f
        let ei, et = if entering then etai, etat else etat, etai

        // Compute sint using Snell's law.
        let sint = ei / (et * (sqrt (max 0.0f (1.0f - cosi*cosi))))
        if (sint >= 1.0f) then
            // Handle total internal reflection.
            Spectrum(1.0f)
        else
            let cost = sqrt (max 0.0f (1.0f - sint*sint))
            fresnelDielectric (abs cosi) cost (Spectrum(ei)) (Spectrum(et))


type FresnelNoOp() =
    inherit Fresnel()

    override this.Evaluate(cosi) = Spectrum(1.0f)


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