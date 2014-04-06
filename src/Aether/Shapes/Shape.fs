namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


type DifferentialGeometry(point : Point, dpdu : Vector, dpdv : Vector,
                            dndu : Normal, dndv : Normal, u : single, v : single,
                            shape : Shape) =
    let normal = Vector.Cross(dpdu, dpdv) |> Vector.Normalize |> Vector.ToNormal
    let normal' = if shape.ReverseOrientation <> shape.TransformSwapsHandedness then normal * -1.0f else normal

    member val Point = point with get, set
    member val Normal = normal' with get, set
    member val U = u with get, set
    member val V = v with get, set
    member val DpDu = dpdu with get, set
    member val DpDv = dpdv with get, set
    member this.DnDu = dndu
    member this.DnDv = dndv
    member this.Shape = shape

    member val DpDx = Vector.Zero with get, set
    member val DpDy = Vector.Zero with get, set
    member val DuDx = 0.0f with get, set
    member val DvDx = 0.0f with get, set
    member val DuDy = 0.0f with get, set
    member val DvDy = 0.0f with get, set

    member this.ComputeDifferentials ray =
        () // TODO

    member this.Clone() =
        let result = DifferentialGeometry(this.Point, this.DpDu, this.DpDv,
                                            this.DnDu, this.DnDv, this.U, this.V,
                                            shape)
        result.DpDx <- this.DpDx
        result.DpDy <- this.DpDy
        result.DuDx <- this.DuDx
        result.DvDx <- this.DvDx
        result.DuDy <- this.DuDy
        result.DvDy <- this.DvDy
        result


and [<AbstractClass>] Shape(objectToWorld : Transform, reverseOrientation) =

    let worldToObject = Transform.Inverse objectToWorld
    let transformSwapsHandedness = objectToWorld.SwapsHandedness()

    abstract ObjectSpaceBounds: BBox

    abstract WorldSpaceBounds: BBox
    default this.WorldSpaceBounds = 
        objectToWorld |>> this.ObjectSpaceBounds

    member this.ObjectToWorld = objectToWorld
    member this.WorldToObject = worldToObject

    abstract GetShadingGeometry : Transform * DifferentialGeometry -> DifferentialGeometry
    default this.GetShadingGeometry(obj2World, dg) = dg

    member this.TransformSwapsHandedness = transformSwapsHandedness
    member this.ReverseOrientation = reverseOrientation

    abstract CanIntersect : bool
    default this.CanIntersect = true

    abstract Refine : unit -> Shape list
    default this.Refine() =
        failwith "Unimplemented Shape.Refine() method called"

    abstract TryIntersect : RaySegment -> (single * single * DifferentialGeometry) option
    default this.TryIntersect(ray) =
        failwith "Unimplemented Shape.TryIntersect() method called"

    abstract Intersects : RaySegment -> bool
    default this.Intersects(ray) =
        failwith "Unimplemented Shape.Intersects() method called"

    abstract Area : unit -> single
    default this.Area() =
        failwith "Unimplemented Shape.Area() method called"

    abstract Sample : single * single -> Point * Normal
    default this.Sample(u1, u2) =
        failwith "Unimplemented Shape.Sample() method called"

    abstract Pdf : Point -> single
    default this.Pdf(pShape) =
        1.0f / this.Area()

    abstract Sample : Point * single * single -> Point * Normal
    default this.Sample(p, u1, u2) =
        this.Sample(u1, u2)

    abstract Pdf : Point * Vector -> single
    default this.Pdf(p, wi) =
        // Intersect sample ray with area light geometry.
        let ray = RaySegment(p, wi, 1e-3f)
        ray.Depth <- -1 // temporary hack to ignore alpha mask.
        match this.TryIntersect(ray) with
        | Some(thit, _, dgLight) ->
            // Convert light sample weight to solid angle measure
            let pdf = Point.DistanceSquared(p, ray.Evaluate(thit)) /
                        (Normal.AbsDot(dgLight.Normal, -wi) * this.Area())
            if System.Single.IsInfinity(pdf) then
                0.0f
            else
                pdf
        | None -> 0.0f


type TextureCoordinate = { X : single; Y : single }


type ITexture<'T> =
    abstract member Evaluate : DifferentialGeometry -> 'T