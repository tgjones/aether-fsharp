namespace Aether.Materials

open System.Runtime.InteropServices
open Aether
open Aether.Math
open Aether.Geometry
open Aether.Shapes
open Aether.Reflection
open Aether.Textures


module MaterialUtilities =
    let bump (d : Texture<single>) (dgGeom : DifferentialGeometry) (dgs : DifferentialGeometry) =
        // Compute offset positions and evaluate displacement texture.
        let dgEval = dgs.Clone()

        // Shift dgEval du in the u direction.
        let duTemp = 0.5f * (abs(dgs.DuDx) + abs(dgs.DuDy))
        let du = if duTemp = 0.0f then 0.01f else duTemp
        dgEval.Point <- dgs.Point + du * dgs.DpDu
        dgEval.U <- dgs.U + du
        dgEval.Normal <- Normal.Normalize(Vector.Cross(dgs.DpDu, dgs.DpDv).ToNormal() + du * dgs.DnDu)
        let uDisplace = d.Evaluate(dgEval)

        // Shift dgEval dv in the v direction.
        let dvTemp = 0.5f * (abs(dgs.DvDx) + abs(dgs.DvDy))
        let dv = if dvTemp = 0.0f then 0.01f else dvTemp
        dgEval.Point <- dgs.Point + dv * dgs.DpDv
        dgEval.U <- dgs.U
        dgEval.V <- dgs.V + dv
        dgEval.Normal <- Normal.Normalize(Vector.Cross(dgs.DpDu, dgs.DpDv).ToNormal() + dv * dgs.DnDv)
        let vDisplace = d.Evaluate(dgEval)

        let displace = d.Evaluate(dgs)

        // Compute bump-mapped differential geometry
        let dgBump = dgs.Clone()
        dgBump.DpDu <- dgs.DpDu + (uDisplace - displace) / du * dgs.Normal.ToVector() + displace * dgs.DnDu.ToVector()
        dgBump.DpDv <- dgs.DpDv + (vDisplace - displace) / dv * dgs.Normal.ToVector() + displace * dgs.DnDv.ToVector()
        dgBump.Normal <- Vector.Cross(dgBump.DpDu, dgBump.DpDv) |> Vector.Normalize |> Vector.ToNormal
        if dgs.Shape.ReverseOrientation <> dgs.Shape.TransformSwapsHandedness then
            dgBump.Normal <- dgBump.Normal * -1.0f

        // Orient shading normal to match geometric normal
        dgBump.Normal <- Normal.FaceForward(dgBump.Normal, dgGeom.Normal)

        dgBump


[<AbstractClass>]
type Material() =
    abstract member GetBsdf: DifferentialGeometry * DifferentialGeometry -> Bsdf


type MatteMaterial(kd : Texture<Spectrum>,
                   sigma : Texture<single>, 
                   bumpMap : Texture<single> option) =
    inherit Material()

    override this.GetBsdf(dgGeom, dgShading) =
        let dgs =
            match bumpMap with
            | Some(x) -> MaterialUtilities.bump x dgGeom dgShading
            | None -> dgShading

        let bsdf = Bsdf(dgs, dgGeom.Normal)

        // Evaluate textures for material.
        let r = kd.Evaluate(dgs) |> Spectrum.Clamp
        let sigm = clamp (sigma.Evaluate(dgs)) 0.0f 90.0f
        
        if not (r.IsBlack()) then
            if sigm = 0.0f then
                bsdf.Add(Lambertian(r))
            else
                bsdf.Add(OrenNayar(r, sigm))

        bsdf