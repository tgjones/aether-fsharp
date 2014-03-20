namespace Aether.Materials

open System.Runtime.InteropServices
open Aether
open Aether.Math
open Aether.Shapes
open Aether.Reflection
open Aether.Textures


[<AbstractClass>]
type Material() =
    abstract member GetBsdf: DifferentialGeometry -> Bsdf


type MatteMaterial(kd : Texture<Spectrum>,
                   sigma : Texture<single>, 
                   bumpMap : Texture<single> option) =
    inherit Material()

    override this.GetBsdf dg =
        // TODO: Bump mapping.

        let bsdf = Bsdf(dg, dg.Normal)

        // Evaluate textures for material.
        let r = kd.Evaluate(dg) |> Spectrum.Clamp
        let sigm = clamp (sigma.Evaluate(dg)) 0.0f 90.0f
        
        if not (r.IsBlack()) then
            if sigm = 0.0f then
                bsdf.Add(Lambertian(r))
            else
                bsdf.Add(OrenNayar(r, sigm))

        bsdf