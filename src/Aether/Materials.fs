namespace Aether.Materials

open Aether.Shapes
open Aether.Reflection


[<AbstractClass>]
type Material() =
    abstract member GetBsdf: DifferentialGeometry -> Bsdf


type MatteMaterial(kd) =
    inherit Material()

    override this.GetBsdf dg =
        let bsdf = Bsdf(dg, dg.Normal)
        bsdf.Add(Lambertian(kd))
        bsdf

