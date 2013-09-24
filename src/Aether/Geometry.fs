namespace Aether.Geometry

open Aether.Math


type IVector =
    abstract X : single
    abstract Y : single
    abstract Z : single


type Vector(x : single, y : single, z : single) =
    member this.X = x
    member this.Y = y
    member this.Z = z

    member this.Item
        with get(index) =
            match index with
            | 0 -> x
            | 1 -> y
            | 2 -> z
            | _ -> failwith "Invalid index"

    interface IVector with
        member this.X = x
        member this.Y = y
        member this.Z = z

    static member (+) (v1 : Vector, v2 : Vector) =
        Vector(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z)

    static member (-) (v1 : Vector, v2 : Vector) =
        Vector(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z)

    static member (*) (v : Vector, f) =
        Vector(v.X * f, v.Y * f, v.Z * f)

    static member (*) (f, v : Vector) =
        Vector(v.X * f, v.Y * f, v.Z * f)

    static member (/) (v : Vector, f) =
        Vector(v.X / f, v.Y / f, v.Z / f)

    static member (~-) (v : Vector) =
        Vector(-v.X, -v.Y, -v.Z)

    override this.Equals(other) =
        match other with
        | :? Vector as p2 -> this.X = p2.X && this.Y = p2.Y && this.Z = p2.Z
        | _ -> false


and Point(x : single, y : single, z : single) =
    
    member this.X = x
    member this.Y = y
    member this.Z = z

    member this.Item
        with get(index) =
            match index with
            | 0 -> x
            | 1 -> y
            | 2 -> z
            | _ -> failwith "Invalid index"

    static member (+) (p1 : Point, p2 : Point) =
        Point(p1.X + p2.X, p1.Y + p2.Y, p1.Z + p2.Z)

    static member (+) (p : Point, v : Vector) =
        Point(p.X + v.X, p.Y + v.Y, p.Z + v.Z)

    static member (-) (p1 : Point, p2 : Point) =
        Vector(p1.X - p2.X, p1.Y - p2.Y, p1.Z - p2.Z)

    static member (-) (p : Point, v : Vector) =
        Point(p.X - v.X, p.Y - v.Y, p.Z - v.Z)

    static member (*) (p : Point, f) =
        Point(p.X * f, p.Y * f, p.Z * f)

    static member (*) (f, p : Point) =
        Point(p.X * f, p.Y * f, p.Z * f)

    static member (/) (p : Point, f) =
        Point(p.X / f, p.Y / f, p.Z / f)

    override this.Equals(other) =
        match other with
        | :? Point as p2 -> this.X = p2.X && this.Y = p2.Y && this.Z = p2.Z
        | _ -> false


and Normal(x : single, y : single, z : single) =
    
    member this.X = x
    member this.Y = y
    member this.Z = z

    interface IVector with
        member this.X = x
        member this.Y = y
        member this.Z = z

    static member (+) (n1 : Normal, n2 : Normal) =
        Normal(n1.X + n2.X, n1.Y + n2.Y, n1.Z + n2.Z)

    static member (-) (n1 : Normal, n2 : Normal) =
        Normal(n1.X - n2.X, n1.Y - n2.Y, n1.Z - n2.Z)

    static member (*) (n : Normal, f) =
        Normal(n.X * f, n.Y * f, n.Z * f)

    static member (*) (f, n : Normal) =
        Normal(n.X * f, n.Y * f, n.Z * f)

    static member (/) (n : Normal, f) =
        Normal(n.X / f, n.Y / f, n.Z / f)

    override this.Equals(other) =
        match other with
        | :? Normal as p2 -> this.X = p2.X && this.Y = p2.Y && this.Z = p2.Z
        | _ -> false


[<AutoOpen>]
module VectorOperations =

    let inline dot (v1 : #IVector) (v2 : #IVector) =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

    let inline absdot (v1 : #IVector) (v2 : #IVector) =
        abs (v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z)

    let inline cross (v1 : #IVector) (v2 : #IVector) =
        Vector(v1.Y * v2.Z - v1.Z * v2.Y,
               v1.Z * v2.X - v1.X * v2.Z,
               v1.X * v2.Y - v1.Y * v2.X)

    let inline faceForward v1 v2 =
        if (dot v1 v2) < 0.0f then -v1 else v1


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Vector =

    let inline lengthSq (v : Vector) =
        v.X * v.X + v.Y * v.Y + v.Z * v.Z

    let inline length v =
        sqrt (lengthSq v)

    let inline normalize v =
        let l = length v
        Vector(v.X / l, v.Y / l, v.Z / l)
        
    let inline sphericalDirection sinTheta cosTheta phi =
        Vector(sinTheta * (cos phi), sinTheta * (sin phi), cosTheta)

    let inline sphericalDirectionVector sinTheta cosTheta phi (x : Vector) (y : Vector) (z : Vector) =
        (sinTheta * (cos phi) * x) + (sinTheta * (sin phi) * y) + (cosTheta * z)

    let inline sphericalTheta (v : Vector) =
        acos (clamp v.Z -1.0f 1.0f)

    let inline sphericalPhi (v : Vector) =
        let p = atan2 v.Y v.X

        if p < 0.0f then p + 2.0f * pi
        else p

    [<CompiledName("Zero")>]
    let zero = Vector(0.0f, 0.0f, 0.0f)

    let inline toNormal (v : Vector) =
        Normal(v.X, v.Y, v.Z)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Point =

    let distance (p1 : Point) (p2 : Point) =
        Vector.length (p1 - p2)

    let distanceSq (p1 : Point) (p2 : Point) =
        Vector.lengthSq (p1 - p2)

    [<CompiledName("Zero")>]
    let zero = Point(0.0f, 0.0f, 0.0f)

    let inline toVector (p : Point) =
        Vector(p.X, p.Y, p.Z)



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Normal =

    let inline lengthSq (v : Normal) =
        v.X * v.X + v.Y * v.Y + v.Z * v.Z

    let inline length v =
        sqrt (lengthSq v)

    let inline normalize v =
        let l = length v
        Normal(v.X / l, v.Y / l, v.Z / l)


type RaySegment(origin : Point, direction : Vector,
                minT : single, maxT : single,
                ?time : single) =

    let mutable maxT' = maxT
    let time' = defaultArg time 0.0f

    member this.Origin = origin
    member this.Direction = direction
    member this.MinT = minT
    member this.MaxT
        with get () = maxT'
        and set (value) = maxT' <- value
    member this.Time = time'


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module RaySegment =

    let evaluate t (ray : RaySegment) =
        ray.Origin + ray.Direction * t

    let withTime time (ray : RaySegment) =
        RaySegment(ray.Origin, ray.Direction, ray.MinT, ray.MaxT, time)


type BBox(min, max) =
    member this.Min = min
    member this.Max = max


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module BBox =
    let empty = let i = infinityf in BBox(Point(i, i, i), Point(-i, -i, -i))

    let fromPoints (p1 : Point) (p2 : Point) =
        BBox(Point(min p1.X p2.X, min p1.Y p2.Y, min p1.Z p2.Z),
             Point(max p1.X p2.X, max p1.Y p2.Y, max p1.Z p2.Z))
    
    let fromPoint p = fromPoints p p

    let unionBoxPoint (b : BBox) (p : Point) =
        let min = Point(min b.Min.X p.X, min b.Min.Y p.Y, min b.Min.Z p.Z)
        let max = Point(max b.Max.X p.X, max b.Max.Y p.Y, max b.Max.Z p.Z)
        BBox(min, max)

    let fromPointList (points : Point list) =
        let mutable result = empty
        for point in points do
            result <- unionBoxPoint result point
        result

    let unionBoxBox (b1 : BBox) (b2 : BBox) =
        let min = Point(min b1.Min.X b2.Min.X, min b1.Min.Y b2.Min.Y, min b1.Min.Z b2.Min.Z)
        let max = Point(max b1.Max.X b2.Max.X, max b1.Max.Y b2.Max.Y, max b1.Max.Z b2.Max.Z)
        BBox(min, max)

    let overlaps (b1 : BBox) (b2 : BBox) =
        let x = b1.Max.X >= b2.Min.X && b1.Min.X <= b2.Max.X
        let y = b1.Max.Y >= b2.Min.Y && b1.Min.Y <= b2.Max.Y
        let z = b1.Max.Z >= b2.Min.Z && b1.Min.Z <= b2.Max.Z
        x && y && z

    let inside (b : BBox) (p : Point) =
        p.X >= b.Min.X && p.X <= b.Max.X &&
        p.Y >= b.Min.Y && p.Y <= b.Max.Y &&
        p.Z >= b.Min.Z && p.Z <= b.Max.Z

    let expand (b : BBox) delta =
        BBox(b.Min - Vector(delta, delta, delta),
             b.Max + Vector(delta, delta, delta))

    let diag (b : BBox) =
        b.Max - b.Min

    let surfaceArea b =
        let d = diag b
        2.0f * (d.X * d.Y + d.X * d.Z + d.Y * d.Z)

    let volume b =
        let d = diag b
        d.X * d.Y * d.Z

    let maximumExtent b =
        let d = diag b
        if d.X > d.Y && d.X > d.Z then 0
        else if d.Y > d.Z then 1
        else 2

    let boundingSphere (b : BBox) =
        let center = 0.5f * b.Min + 0.5f * b.Max
        let radius = if inside b center then Point.distance center b.Max else 0.0f
        (center, radius)

    let tryIntersect (b : BBox) (ray : RaySegment) =
        let t0 = ref ray.MinT
        let t1 = ref ray.MaxT

        let testDimension i =
            // Update interval for bounding box slab with specified index.
            let invRayDir = 1.0f / ray.Direction.[i]
            let mutable tNear = (b.Min.[i] - ray.Origin.[i]) * invRayDir
            let mutable tFar  = (b.Max.[i] - ray.Origin.[i]) * invRayDir

            // Update parametric interval from slab intersection.
            if tNear > tFar then swap &tNear &tFar
            t0 := if tNear > !t0 then tNear else !t0
            t1 := if tFar  < !t1 then tFar  else !t1

            t0 <= t1

        let falseResult = (false, None, None)

        if not (testDimension 0) then falseResult
        else if not (testDimension 1) then falseResult
        else if not (testDimension 2) then falseResult
        else (true, Some(!t0), Some(!t1))


        