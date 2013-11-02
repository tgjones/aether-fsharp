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