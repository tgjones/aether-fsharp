namespace Aether.Primitives

open Nexus.Graphics.Transforms
open Aether.Math
open Aether.Materials
open Aether.Shapes


type Intersection(primitive : Primitive, dg : DifferentialGeometry, worldToObject) =
    member this.GetBsdf ray =
        dg.ComputeDifferentials(ray)
        primitive.GetBsdf dg worldToObject


and [<AbstractClass>] Primitive() =
    abstract TryIntersect : RaySegment3D -> (bool * option<Intersection>)
    abstract Intersects : RaySegment3D -> bool

    abstract GetBsdf : DifferentialGeometry -> Transform3D -> Bsdf


type GeometricPrimitive(shape : Shape, material : Material) =
    inherit Primitive()

    override this.TryIntersect ray =
        let (result, tHit, dg) = shape.TryIntersect(ray)
        if not(result) then
            (false, None)
        else
            let intersection = Intersection(this, Option.get dg, shape.WorldToObject)
            ray.MaxT <- tHit
            (true, Some(intersection))

    override this.Intersects ray =
        shape.Intersects(ray)

    override this.GetBsdf dg worldToObject =
        // TODO: Allow shape to use a different geometry for shading.
        material.GetBsdf(dg)