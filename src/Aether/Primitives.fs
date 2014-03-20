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

    member this.GetBsdf ray =
        dg.ComputeDifferentials(ray)
        primitive.GetBsdf dg worldToObject


and [<AbstractClass>] Primitive() =

  abstract WorldBound : unit -> BBox

  member this.FullyRefine() =
    let refined = List<Primitive>()
    let todo = Queue([ this ])
    while todo.Count > 0 do
      let primitive = todo.Dequeue()
      if primitive.CanIntersect() then
        refined.Add(primitive)
      else
        primitive.Refine() |> List.iter (fun x -> refined.Add(x))
    List.ofSeq refined

  abstract CanIntersect : unit -> bool
  default this.CanIntersect() = true

  abstract Refine : unit -> Primitive list
  default this.Refine() = failwith "Unimplemented method called!"

  abstract TryIntersect : RaySegment -> Intersection option
  abstract Intersects : RaySegment -> bool

  abstract GetBsdf : DifferentialGeometry -> Transform -> Bsdf


type GeometricPrimitive(shape : Shape, material : Material) =
  inherit Primitive()

  override this.WorldBound() =
    shape.WorldSpaceBounds

  override this.CanIntersect() =
    match shape with
    | :? IntersectableShape -> true
    | _ -> false

  override this.Refine() =
    match shape with
    | :? RefinableShape as s ->
      s.Refine() |> List.map (fun x -> GeometricPrimitive(x, material) :> Primitive)
    | _ -> failwith "Shape is not refinable"

  override this.TryIntersect ray =
    match shape with
    | :? IntersectableShape as s ->
      match s.TryIntersect(ray) with
      | Some(tHit, rayEpsilon, dg) ->
        let intersection = Intersection(this, dg, shape.WorldToObject, rayEpsilon)
        ray.MaxT <- tHit
        Some(intersection)
      | None -> None
    | _ -> failwith "Shape is not intersectable"

  override this.Intersects ray =
    match shape with
    | :? IntersectableShape as s -> s.Intersects(ray)
    | _ -> failwith "Shape is not intersectable"

  override this.GetBsdf dg worldToObject =
    // TODO: Allow shape to use a different geometry for shading.
    material.GetBsdf(dg)


[<AbstractClass>]
type Aggregate() =
  inherit Primitive()

  override this.GetBsdf dg worldToObject =
    failwith "Should have gone to GeometricPrimitive"