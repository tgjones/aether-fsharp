namespace Aether.Shapes

open Aether.Math
open Aether.Geometry
open Aether.Transforms


type DifferentialGeometry(point : Point, dpdu : Vector, dpdv : Vector,
                          dndu, dndv, u, v, shape : Shape) =
    let normal = Vector.Cross(dpdu, dpdv) |> Vector.Normalize |> Vector.ToNormal
    let normal' = if shape.ReverseOrientation <> shape.TransformSwapsHandedness then normal * -1.0f else normal

    member this.Point = point
    member this.Normal = normal'
    member this.DpDu = dpdu

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

    abstract TryIntersect : RaySegment -> (bool * single * single * option<DifferentialGeometry>)

    abstract Intersects : RaySegment -> bool
    default this.Intersects ray =
        let result, _, _, _ = this.TryIntersect(ray)
        result


and [<AbstractClass>] RefinableShape(objectToWorld, reverseOrientation) =
    inherit Shape(objectToWorld, reverseOrientation)

    abstract Refine : unit -> Shape list


type Plane(objectToWorld, reverseOrientation, point, normal) =
    inherit IntersectableShape(objectToWorld, reverseOrientation)

    member this.Point = point
    member this.Normal = normal

    override this.ObjectSpaceBounds =
        raise (System.NotImplementedException())

    override this.TryIntersect ray =
        raise (System.NotImplementedException())


type TextureCoordinate = { X : single; Y : single }