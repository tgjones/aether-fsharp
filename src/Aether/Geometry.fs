namespace Aether.Geometry

open Aether.Math


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

    /// Square of the length of the vector.
    member inline this.LengthSquared() =
        this.X * this.X + this.Y * this.Y + this.Z * this.Z

    /// Length of the vector.
    member inline this.Length() =
        sqrt (this.LengthSquared())

    /// Normalizes the given vector to unit length.
    static member inline Normalize (v : Vector) =
        let l = v.Length()
        Vector(v.X / l, v.Y / l, v.Z / l)

    /// Calculates the dot product.
    static member inline Dot(v1 : Vector, v2 : Vector) =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

    /// Calculates the dot product.
    static member inline Dot(v1 : Vector, v2 : Normal) =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

    /// Calculates the dot product, and then calls abs on the result.
    static member inline AbsDot(v1 : Vector, v2 : Vector) =
        abs (Vector.Dot(v1, v2))

    /// Calculates the dot product, and then calls abs on the result.
    static member inline AbsDot(v1 : Vector, v2 : Normal) =
        abs (Vector.Dot(v1, v2))

    /// Calculates the cross product.
    static member inline Cross(v1 : Vector, v2 : Vector) =
        Vector(v1.Y * v2.Z - v1.Z * v2.Y,
               v1.Z * v2.X - v1.X * v2.Z,
               v1.X * v2.Y - v1.Y * v2.X)

    /// Calculates the cross product.
    static member inline Cross(v1 : Vector, v2 : Normal) =
        Vector(v1.Y * v2.Z - v1.Z * v2.Y,
               v1.Z * v2.X - v1.X * v2.Z,
               v1.X * v2.Y - v1.Y * v2.X)

    /// Creates a coordinate system from a single vector, using the cross
    /// product two times to get a set of three orthogonal vectors.
    static member CoordinateSystem (v1 : Vector) =
        let v2 =
            if abs v1.X > abs v1.Y then
                let invLen = 1.0f / (sqrt v1.X*v1.X + v1.Z*v1.Z)
                Vector(-v1.Z * invLen, 0.0f, v1.X * invLen)
            else
                let invLen = 1.0f / (sqrt v1.Y*v1.Y + v1.Z*v1.Z)
                Vector(0.0f, v1.Z * invLen, -v1.Y * invLen)
        let v3 = Vector.Cross(v1, v2)
        (v2, v3)

    /// Flips a surface normal (v1) so that it lies in the same hemisphere as a
    /// given vector (v2).
    static member inline FaceForward(v1 : Vector, v2 : Vector) =
        if Vector.Dot(v1, v2) < 0.0f then -v1 else v1

    /// Flips a surface normal (v1) so that it lies in the same hemisphere as a
    /// given vector (v2).
    static member inline FaceForward(v1 : Vector, v2 : Normal) =
        if Vector.Dot(v1, v2) < 0.0f then -v1 else v1
        
    /// Converts spherical coordinates (theta and phi) into a direction vector.
    static member inline SphericalDirection(sinTheta, cosTheta, phi) =
        Vector(sinTheta * (cos phi), sinTheta * (sin phi), cosTheta)

    /// Converts spherical coordinates (theta and phi) into a direction vector
    /// with respect to the coordinate frame defined by the x, y and z vectors.
    static member inline SphericalDirection(sinTheta, cosTheta, phi,
                                            x : Vector, y : Vector, z : Vector) =
        (sinTheta * (cos phi) * x) + (sinTheta * (sin phi) * y) + (cosTheta * z)

    /// Converts a vector direction to a spherical theta angle.
    static member inline SphericalTheta (v : Vector) =
        acos (clamp v.Z -1.0f 1.0f)

    /// Converts a vector direction to a spherical phi angle.
    static member inline SphericalPhi (v : Vector) =
        let p = atan2 v.Y v.X
        if p < 0.0f then p + 2.0f * pi
        else p

    /// Converts a vector direction to spherical coordinates (theta, phi).
    static member inline SphericalAngles (v : Vector) =
        (Vector.SphericalTheta v, Vector.SphericalPhi v)

    /// Vector(0.0f, 0.0f, 0.0f)
    static member Zero = Vector(0.0f, 0.0f, 0.0f)

    /// Converts a Vector to a Normal.
    member inline this.ToNormal () =
        Normal(this.X, this.Y, this.Z)

    /// Converts a Vector to a Normal.
    static member inline ToNormal (v : Vector) =
        v.ToNormal()

    override this.Equals(other) =
        match other with
        | :? Vector as p2 -> this.X = p2.X && this.Y = p2.Y && this.Z = p2.Z
        | _ -> false

    override this.GetHashCode() =
        let mutable hashCode = this.X.GetHashCode()
        hashCode <- (hashCode * 397) ^^^ this.Y.GetHashCode()
        hashCode <- (hashCode * 397) ^^^ this.Z.GetHashCode()
        hashCode

    override this.ToString() =
        sprintf "{X:%f Y:%f Z:%f}" this.X this.Y this.Z


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

    static member (~-) (v : Point) =
        Point(-v.X, -v.Y, -v.Z)

    /// Calculates the distance between two points.
    static member Distance(p1 : Point, p2 : Point) =
        (p1 - p2).Length()

    /// Calculates the square of the distance between two points.
    static member DistanceSquared(p1 : Point, p2 : Point) =
        (p1 - p2).LengthSquared()

    /// Point(0.0f, 0.0f, 0.0f)
    static member Zero = Point(0.0f, 0.0f, 0.0f)

    /// Converts a Point to a Vector.
    member inline this.ToVector () =
        Vector(this.X, this.Y, this.Z)

    /// Converts a Point to a Vector.
    static member inline ToVector (p : Point) =
        p.ToVector()

    override this.Equals(other) =
        match other with
        | :? Point as p2 -> this.X = p2.X && this.Y = p2.Y && this.Z = p2.Z
        | _ -> false

    override this.GetHashCode() =
        let mutable hashCode = this.X.GetHashCode()
        hashCode <- (hashCode * 397) ^^^ this.Y.GetHashCode()
        hashCode <- (hashCode * 397) ^^^ this.Z.GetHashCode()
        hashCode

    override this.ToString() =
        sprintf "{X:%f Y:%f Z:%f}" this.X this.Y this.Z


and Normal(x : single, y : single, z : single) =
    
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

    static member (~-) (v : Normal) =
        Normal(-v.X, -v.Y, -v.Z)

    /// Square of the length of the normal.
    member inline this.LengthSquared() =
        this.X * this.X + this.Y * this.Y + this.Z * this.Z

    /// Length of the normal.
    member inline this.Length() =
        sqrt (this.LengthSquared())

    /// Normalizes the given normal to unit length.
    static member inline Normalize(v : Normal) =
        let l = v.Length()
        Normal(v.X / l, v.Y / l, v.Z / l)

    /// Calculates the dot product.
    static member inline Dot(v1 : Normal, v2 : Normal) =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

    /// Calculates the dot product.
    static member inline Dot(v1 : Normal, v2 : Vector) =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

    /// Calculates the dot product, and then calls abs on the result.
    static member inline AbsDot(v1 : Normal, v2 : Normal) =
        abs (Normal.Dot(v1, v2))

    /// Calculates the dot product, and then calls abs on the result.
    static member inline AbsDot(v1 : Normal, v2 : Vector) =
        abs (Normal.Dot(v1, v2))

    /// Calculates the cross product.
    static member inline Cross(v1 : Normal, v2 : Vector) =
        Vector(v1.Y * v2.Z - v1.Z * v2.Y,
               v1.Z * v2.X - v1.X * v2.Z,
               v1.X * v2.Y - v1.Y * v2.X)

    /// Flips a surface normal (v1) so that it lies in the same hemisphere as a
    /// given vector (v2).
    static member inline FaceForward(v1 : Normal, v2 : Normal) =
        if Normal.Dot(v1, v2) < 0.0f then -v1 else v1

    /// Flips a surface normal (v1) so that it lies in the same hemisphere as a
    /// given vector (v2).
    static member inline FaceForward(v1 : Normal, v2 : Vector) =
        if Normal.Dot(v1, v2) < 0.0f then -v1 else v1

    override this.Equals(other) =
        match other with
        | :? Normal as p2 -> this.X = p2.X && this.Y = p2.Y && this.Z = p2.Z
        | _ -> false

    override this.GetHashCode() =
        let mutable hashCode = this.X.GetHashCode()
        hashCode <- (hashCode * 397) ^^^ this.Y.GetHashCode()
        hashCode <- (hashCode * 397) ^^^ this.Z.GetHashCode()
        hashCode

    override this.ToString() =
        sprintf "{X:%f Y:%f Z:%f}" this.X this.Y this.Z


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

    /// Gets the point at a particular position along a ray.
    member this.Evaluate t =
        this.Origin + this.Direction * t


type BBox(min, max) =
    member this.Min = min
    member this.Max = max

    /// Empty bounding box.
    static member Empty = let i = infinityf in BBox(Point(i, i, i), Point(-i, -i, -i))

    /// Creates a new bounding box large enough to contain both the original
    /// bounding box and the given point.
    static member Union(b : BBox, p : Point) =
        let min = Point(min b.Min.X p.X, min b.Min.Y p.Y, min b.Min.Z p.Z)
        let max = Point(max b.Max.X p.X, max b.Max.Y p.Y, max b.Max.Z p.Z)
        BBox(min, max)

    /// Creates a new bounding box large enough to contain the given two
    /// bounding boxes.
    static member Union(b1 : BBox, b2 : BBox) =
        let min = Point(min b1.Min.X b2.Min.X, min b1.Min.Y b2.Min.Y, min b1.Min.Z b2.Min.Z)
        let max = Point(max b1.Max.X b2.Max.X, max b1.Max.Y b2.Max.Y, max b1.Max.Z b2.Max.Z)
        BBox(min, max)

    /// Creates a bounding box from two points.
    static member FromPoints(p1 : Point, p2 : Point) =
        BBox(Point(min p1.X p2.X, min p1.Y p2.Y, min p1.Z p2.Z),
             Point(max p1.X p2.X, max p1.Y p2.Y, max p1.Z p2.Z))
    
    /// Creates a bounding box from one point.
    static member FromPoint p = BBox.FromPoints(p, p)

    /// Creates a bounding box from the given point list.
    static member FromPoints(points : Point list) =
        let mutable result = BBox.Empty
        for point in points do
            result <- BBox.Union(result, point)
        result

    /// Returns true if any part of the two bounding boxes overlap.
    member this.Overlaps (b2 : BBox) =
        let x = this.Max.X >= b2.Min.X && this.Min.X <= b2.Max.X
        let y = this.Max.Y >= b2.Min.Y && this.Min.Y <= b2.Max.Y
        let z = this.Max.Z >= b2.Min.Z && this.Min.Z <= b2.Max.Z
        x && y && z

    /// Returns true if the point is inside the bounding box.
    member this.Inside (p : Point) =
        p.X >= this.Min.X && p.X <= this.Max.X &&
        p.Y >= this.Min.Y && p.Y <= this.Max.Y &&
        p.Z >= this.Min.Z && p.Z <= this.Max.Z

    /// Creates a new bounding box padded by the given amount.
    static member Expand (b : BBox) delta =
        BBox(b.Min - Vector(delta, delta, delta),
             b.Max + Vector(delta, delta, delta))

    /// Returns the diagonal distance between the min and max.
    member private this.Diag() =
        this.Max - this.Min

    /// Returns the surface area of the bounding box faces.
    member this.SurfaceArea() =
        let d = this.Diag()
        2.0f * (d.X * d.Y + d.X * d.Z + d.Y * d.Z)

    /// Returns the volume inside the bounding box.
    member this.Volume() =
        let d = this.Diag()
        d.X * d.Y * d.Z

    /// Returns the largest axis (X=0, Y=1, Z=2) of the bounding box.
    member this.MaximumExtent() =
        let d = this.Diag()
        if d.X > d.Y && d.X > d.Z then 0
        else if d.Y > d.Z then 1
        else 2

    /// Returns the center and radius of a sphere that bounds the given bounding box.
    member this.BoundingSphere() =
        let center = 0.5f * this.Min + 0.5f * this.Max
        let radius = if this.Inside(center) then Point.Distance(center, this.Max) else 0.0f
        (center, radius)
        
    /// Tests for intersection of the given ray with the given bounding box.
    member this.TryIntersect (ray : RaySegment) =
        let t0 = ref ray.MinT
        let t1 = ref ray.MaxT

        let testDimension i =
            // Update interval for bounding box slab with specified index.
            let invRayDir = 1.0f / ray.Direction.[i]
            let mutable tNear = (this.Min.[i] - ray.Origin.[i]) * invRayDir
            let mutable tFar  = (this.Max.[i] - ray.Origin.[i]) * invRayDir

            // Update parametric interval from slab intersection.
            if tNear > tFar then swap &tNear &tFar
            t0 := if tNear > !t0 then tNear else !t0
            t1 := if tFar  < !t1 then tFar  else !t1

            t0 <= t1

        let falseResult = (None, None)

        if not (testDimension 0) then falseResult
        else if not (testDimension 1) then falseResult
        else if not (testDimension 2) then falseResult
        else (Some(!t0), Some(!t1))    