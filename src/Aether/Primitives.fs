namespace Aether.Primitives

open System.Collections.Generic
open Nexus.Graphics.Transforms
open Aether.Math
open Aether.Reflection
open Aether.Materials
open Aether.Shapes


type Intersection(primitive : Primitive, dg : DifferentialGeometry, worldToObject) =
    member this.GetBsdf ray =
        dg.ComputeDifferentials(ray)
        primitive.GetBsdf dg worldToObject


and [<AbstractClass>] Primitive() =
    member this.FullyRefine() =
        let refined = List<Primitive>()
        let todo = Queue([ this ])
        while todo.Count > 0 do
            let primitive = todo.Dequeue()
            if primitive.CanIntersect() then
                refined.Add(primitive)
            else
                primitive.Refine() |> List.iter (fun x -> refined.Add(x))

    abstract CanIntersect : unit -> bool
    default this.CanIntersect() = true

    abstract Refine : unit -> Primitive list

    abstract TryIntersect : RaySegment3D -> (bool * option<Intersection>)
    abstract Intersects : RaySegment3D -> bool

    abstract GetBsdf : DifferentialGeometry -> Transform3D -> Bsdf


type GeometricPrimitive(shape : Shape, material : Material) =
    inherit Primitive()

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
            let (result, tHit, dg) = s.TryIntersect(ray)
            if not(result) then
                (false, None)
            else
                let intersection = Intersection(this, Option.get dg, shape.WorldToObject)
                ray.MaxT <- tHit
                (true, Some(intersection))
        | _ -> failwith "Shape is not intersectable"

    override this.Intersects ray =
        match shape with
        | :? IntersectableShape as s -> s.Intersects(ray)
        | _ -> failwith "Shape is not intersectable"

    override this.GetBsdf dg worldToObject =
        // TODO: Allow shape to use a different geometry for shading.
        material.GetBsdf(dg)