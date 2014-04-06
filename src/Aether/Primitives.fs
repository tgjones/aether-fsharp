namespace Aether.Primitives

open System.Collections.Generic
open Aether.Math
open Aether.Geometry
open Aether.Transforms
open Aether.Shapes
open Aether.Reflection
open Aether.Materials


type Intersection(primitive : Primitive, dg : DifferentialGeometry,
                  worldToObject, rayEpsilon : single) =
    
    member this.RayEpsilon = rayEpsilon

    member this.GetBsdf(ray) =
        dg.ComputeDifferentials(ray)
        primitive.GetBsdf(dg, worldToObject)


and [<AbstractClass>] Primitive() =

    abstract WorldBound : unit -> BBox

    member this.FullyRefine() =
        let refined = List<Primitive>()
        let todo = Stack([ this ])
        while todo.Count > 0 do
            let primitive = todo.Pop()
            if primitive.CanIntersect() then
                refined.Add(primitive)
            else
                primitive.Refine() |> List.iter (fun x -> todo.Push(x))
        List.ofSeq refined

    abstract CanIntersect : unit -> bool
    default this.CanIntersect() = true

    abstract Refine : unit -> Primitive list
    default this.Refine() = failwith "Unimplemented method called!"

    abstract TryIntersect : RaySegment -> Intersection option
    abstract Intersects : RaySegment -> bool

    abstract GetBsdf : DifferentialGeometry * Transform -> Bsdf
    //abstract GetBssrdf : DifferentialGeometry * Transform -> Bssrdf


type GeometricPrimitive(shape : Shape, material : Material) =
    inherit Primitive()

    override this.WorldBound() =
        shape.WorldSpaceBounds

    override this.CanIntersect() =
        shape.CanIntersect
    
    override this.Refine() =
        shape.Refine() |> List.map (fun x -> GeometricPrimitive(x, material) :> Primitive)

    override this.TryIntersect ray =
        match shape.TryIntersect(ray) with
        | Some(tHit, rayEpsilon, dg) ->
            let intersection = Intersection(this, dg, shape.WorldToObject, rayEpsilon)
            ray.MaxT <- tHit
            Some(intersection)
        | None -> None

    override this.Intersects ray =
        shape.Intersects(ray)

    override this.GetBsdf(dg, objectToWorld) =
        let dgs = shape.GetShadingGeometry(objectToWorld, dg)
        material.GetBsdf(dg, dgs)


[<AbstractClass>]
type Aggregate() =
    inherit Primitive()

    override this.GetBsdf(dg, worldToObject) =
        failwith "Should have gone to GeometricPrimitive"