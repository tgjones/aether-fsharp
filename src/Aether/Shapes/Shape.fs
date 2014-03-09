namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


type DifferentialGeometry(point : Point, dpdu : Vector, dpdv : Vector,
                          dndu, dndv, u : single, v : single,
                          shape : Shape) =
    let normal = Vector.Cross(dpdu, dpdv) |> Vector.Normalize |> Vector.ToNormal
    let normal' = if shape.ReverseOrientation <> shape.TransformSwapsHandedness then normal * -1.0f else normal

    member this.Point = point
    member this.Normal = normal'
    member this.U = u
    member this.V = v
    member this.DpDu = dpdu

    member val DpDx = Vector.Zero with get, set
    member val DpDy = Vector.Zero with get, set
    member val DuDx = 0.0f with get, set
    member val DvDx = 0.0f with get, set
    member val DuDy = 0.0f with get, set
    member val DvDy = 0.0f with get, set

    member this.ComputeDifferentials ray =
        () // TODO


and [<AbstractClass>] Shape(objectToWorld : Transform, reverseOrientation) =

    let worldToObject = Transform.Inverse objectToWorld
    let transformSwapsHandedness = objectToWorld.SwapsHandedness()

    abstract ObjectSpaceBounds: BBox

    abstract WorldSpaceBounds: BBox
    default this.WorldSpaceBounds = 
        objectToWorld |>> this.ObjectSpaceBounds

    member this.ObjectToWorld = objectToWorld
    member this.WorldToObject = worldToObject

    abstract GetShadingGeometry : Transform -> DifferentialGeometry -> DifferentialGeometry
    default this.GetShadingGeometry obj2World dg = dg

    member this.TransformSwapsHandedness = transformSwapsHandedness
    member this.ReverseOrientation = reverseOrientation


and [<AbstractClass>] IntersectableShape(objectToWorld, reverseOrientation) =
    inherit Shape(objectToWorld, reverseOrientation)

    abstract TryIntersect : RaySegment -> (single * single * DifferentialGeometry) option

    abstract Intersects : RaySegment -> bool
    default this.Intersects ray =
        match this.TryIntersect(ray) with
        | Some(_) -> true
        | None -> false


and [<AbstractClass>] RefinableShape(objectToWorld, reverseOrientation) =
    inherit Shape(objectToWorld, reverseOrientation)

    abstract Refine : unit -> Shape list


type TextureCoordinate = { X : single; Y : single }