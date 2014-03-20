namespace Aether.Accelerators

open System.Collections.Generic

open Aether.Math
open Aether.Geometry
open Aether.Primitives


type Voxel() =
  
    let mutable allCanIntersect = false
    let primitives = List<Primitive>()

    let refineIfNecessary() =
        if not allCanIntersect then
            for i = 0 to primitives.Count - 1 do
                let primitive = primitives.[i]
                // Refine primitive if it's not intersectable.
                if not (primitive.CanIntersect()) then
                    let refinedPrimitives = primitive.FullyRefine()
                    if refinedPrimitives.Length = 1 then
                        primitives.[i] <- refinedPrimitives.[0]
                    else
                        primitives.[i] <- GridAccelerator(refinedPrimitives, false)
                allCanIntersect <- true

    member this.AddPrimitive(primitive) =
        primitives.Add(primitive)

    member this.TryIntersect(ray) =
        refineIfNecessary()

        // Loop over primitives in voxel and find intersections.
        let mutable intersection = Option<Intersection>.None
        for primitive in primitives do
            match primitive.TryIntersect(ray) with
            | Some(isect) -> intersection <- Some(isect)
            | None -> ()

        intersection

    member this.Intersects(ray) =
        refineIfNecessary()
        primitives |> Seq.toList |> List.exists (fun p -> p.Intersects(ray))


and GridAccelerator(primitives : Primitive list, refineImmediately) =
    inherit Aggregate()

    // Initialize with primitives for grid.
    let primitives' =
        if refineImmediately then
            primitives |> List.collect (fun p -> p.FullyRefine())
        else
            primitives

    // Compute bounds and choose grid resolution.
    let bounds =
        primitives'
        |> List.fold (fun acc p -> BBox.Union(acc, p.WorldBound())) BBox.Empty
    let delta = bounds.Max - bounds.Min

    // Find voxelsPerUnitDist for grid.
    let maxAxis = bounds.MaximumExtent()
    let invMaxWidth = 1.0f / delta.[maxAxis]
    let cubeRoot = 3.0f * single (System.Math.Pow(float primitives'.Length, 1.0 / 3.0))
    let voxelsPerUnitDist = cubeRoot * invMaxWidth
    let numVoxelsPerAxis = [
        for axis = 0 to 2 do
            let temp = round2int (delta.[axis] * voxelsPerUnitDist)
            yield clamp temp 1 64 
    ]

    // Compute voxel widths and allocate voxels.
    let width = [ for axis = 0 to 2 do yield delta.[axis] / single numVoxelsPerAxis.[axis] ]
    let invWidth = width |> List.map (fun axis -> if axis = 0.0f then 0.0f else 1.0f / axis)
    let numVoxels = numVoxelsPerAxis.[0] * numVoxelsPerAxis.[1] * numVoxelsPerAxis.[2]
  
    let posToVoxel (p : Point) axis =
        let v = int ((p.[axis] - bounds.Min.[axis]) * invWidth.[axis])
        clamp v 0 (numVoxelsPerAxis.[axis] - 1)

    let voxelToPos (p : int) axis =
        bounds.Min.[axis] + (single p) * width.[axis]

    let offset x y z =
        z * numVoxelsPerAxis.[0] * numVoxelsPerAxis.[1] + y * numVoxelsPerAxis.[0] + x

    // Add primitives to grid voxels.
    let initializeVoxels() =
        let voxels = Array.zeroCreate<Voxel> numVoxels
        for i = 0 to primitives'.Length - 1 do
            // Find voxel extent of primitive.
            let pb = primitives'.[i].WorldBound()
            let vmin = [ for axis = 0 to 2 do yield (posToVoxel pb.Min axis) ]
            let vmax = [ for axis = 0 to 2 do yield (posToVoxel pb.Max axis) ]

            // Add primitive to overlapping voxels.
            for z = vmin.[2] to vmax.[2] do
            for y = vmin.[1] to vmax.[1] do
                for x = vmin.[0] to vmax.[0] do
                let o = offset x y z
                if voxels.[o] = Unchecked.defaultof<Voxel> then
                    voxels.[o] <- Voxel()
                // Add primitive to voxel.
                voxels.[o].AddPrimitive(primitives'.[i])

        voxels

    let voxels = initializeVoxels()

    let doIntersection (ray : RaySegment) (callback : Voxel -> bool ref -> unit) =
        let findGridIntersection() =
            if bounds.Inside(ray.Evaluate(ray.MinT)) then
                Some(ray.MinT)
            else
                bounds.TryIntersect(ray)
                |> Option.bind (fun (min, max) -> Some(min))

        // Check ray against overall grid bounds.
        match findGridIntersection() with
        | Some(rayT) ->
            let gridIntersect = ray.Evaluate(rayT)

            // Setup 3D DDA for ray.
            let nextCrossingT = Array.zeroCreate<single> 3
            let deltaT = Array.zeroCreate<single> 3
            let step = Array.zeroCreate<int> 3
            let out = Array.zeroCreate<int> 3
            let pos = Array.zeroCreate<int> 3

            for axis = 0 to 2 do
                // Compute current voxel for axis.
                pos.[axis] <- posToVoxel gridIntersect axis
                if ray.Direction.[axis] >= 0.0f then
                    // Handle ray with positive direction for voxel stepping.
                    nextCrossingT.[axis] <- rayT + ((voxelToPos (pos.[axis] + 1) axis)
                                                    - gridIntersect.[axis]) / ray.Direction.[axis]
                    deltaT.[axis] <- width.[axis] / ray.Direction.[axis]
                    step.[axis] <- 1
                    out.[axis] <- numVoxelsPerAxis.[axis]
                else
                    // Handle ray with negative direction for voxel stepping.
                    nextCrossingT.[axis] <- rayT + ((voxelToPos pos.[axis] axis)
                                                    - gridIntersect.[axis]) / ray.Direction.[axis]
                    deltaT.[axis] <- -width.[axis] / ray.Direction.[axis]
                    step.[axis] <- -1
                    out.[axis] <- -1

            // Walk ray through voxel grid.
            let reachedEnd = ref false
            while not !reachedEnd do
                // Check for intersection in current voxel and advance to next.
                let voxel = voxels.[offset pos.[0] pos.[1] pos.[2]]
                if voxel <> Unchecked.defaultof<Voxel> then
                    callback voxel reachedEnd

                if not !reachedEnd then
                    // Advance to next voxel.

                    // Find stepAxis for stepping to next voxel.
                    let bits = ((if nextCrossingT.[0] < nextCrossingT.[1] then 1 else 0) <<< 2) +
                               ((if nextCrossingT.[0] < nextCrossingT.[2] then 1 else 0) <<< 1) +
                               ((if nextCrossingT.[1] < nextCrossingT.[2] then 1 else 0))
                    let cmpToAxis = [ 2; 1; 2; 1; 2; 2; 0; 0 ]
                    let stepAxis = cmpToAxis.[bits]
                    if ray.MaxT < nextCrossingT.[stepAxis] then
                        reachedEnd := true
                    else
                        pos.[stepAxis] <- pos.[stepAxis] + step.[stepAxis]
                        if pos.[stepAxis] = out.[stepAxis] then
                            reachedEnd := true
                        else
                            nextCrossingT.[stepAxis] <- nextCrossingT.[stepAxis] + deltaT.[stepAxis]

        | None -> ()

    override this.WorldBound() = bounds

    override this.TryIntersect(ray) =
        let intersection = ref Option<Intersection>.None
        doIntersection ray (fun voxel reachedEnd ->
            match voxel.TryIntersect(ray) with
            | Some(isect) -> intersection := Some(isect)
            | None -> ())
        !intersection

    override this.Intersects(ray) =
        let result = ref false
        doIntersection ray (fun voxel reachedEnd ->
            match voxel.Intersects(ray) with
            | true -> result := true
            | false -> ()
            reachedEnd := true)
        !result