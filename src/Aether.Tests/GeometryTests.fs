namespace ``Geometry - Generic vector operations``

    open Xunit
    open Xunit.Extensions
    open FsUnit.Xunit
    open Aether.Geometry

    type ``Given two vectors`` () =
        let v1 = Vector(1.0f, 2.0f, 3.0f)
        let v2 = Vector(4.0f, 5.0f, -6.0f)

        [<Fact>]
        let ``when dot is called, it calculates the dot product`` () =
            dot v1 v2 |> should equal -4.0f

        [<Fact>]
        let ``when absdot is called, it calculates the absolute dot product`` () =
            absdot v1 v2 |> should equal 4.0f

        [<Fact>]
        let ``when cross is called, it calculates the cross product`` () =
            cross v1 v2 |> should equal (Vector(-27.0f, 18.0f, -3.0f))

        [<Fact>]
        let ``when faceForward is called, it flips the surface normal if necessary`` () =
            faceForward v1 v2 |> should equal (Vector(-1.0f, -2.0f, -3.0f))


namespace ``Geometry - Vector``

    open Xunit
    open Xunit.Extensions
    open FsUnit.Xunit
    open Aether.Math
    open Aether.Geometry

    type ``Given a vector`` () =
        let vector = Vector(1.0f, 2.0f, 3.0f)

        [<Fact>]
        let ``its properties return the correct values`` () =
            vector.X |> should equal 1.0f
            vector.Y |> should equal 2.0f
            vector.Z |> should equal 3.0f

        [<Fact>]
        let ``its indexer returns the correct values`` () =
            vector.[0] |> should equal 1.0f
            vector.[1] |> should equal 2.0f
            vector.[2] |> should equal 3.0f
            (fun () -> vector.[3] |> ignore) |> should throw typeof<System.Exception>

        [<Fact>]
        let ``its arithmetic operators work`` () =
            vector + Vector(4.0f, 2.0f, -1.0f) |> should equal (Vector(5.0f, 4.0f, 2.0f))
            vector - Vector(4.0f, 2.0f, -1.0f) |> should equal (Vector(-3.0f, 0.0f, 4.0f))
            vector * 2.0f |> should equal (Vector(2.0f, 4.0f, 6.0f))
            2.0f * vector |> should equal (Vector(2.0f, 4.0f, 6.0f))
            vector / 2.0f |> should equal (Vector(0.5f, 1.0f, 1.5f))
            -vector |> should equal (Vector(-1.0f, -2.0f, -3.0f))

        [<Fact>]
        let ``its equality operator works`` () =
            vector.Equals(Vector(1.0f, 2.0f, 3.0f)) |> should be True
            vector = Vector(1.0f, 2.0f, 3.0f) |> should be True
            vector.Equals(Vector(1.1f, 2.0f, 3.0f)) |> should be False
            vector = Vector(1.1f, 2.0f, 3.0f) |> should be False

        [<Fact>]
        let ``lengthSq calculates the square of the length of a vector`` () =
            Vector.lengthSq vector |> should equal 14.0f

        [<Fact>]
        let ``length calculates the length of a vector`` () =
            Vector.length vector |> should (equalWithin 0.01f) 3.74f

        [<Fact>]
        let ``normalize normalizes a vector to unit length`` () =
            let normalized = Vector.normalize vector
            normalized.X |> should (equalWithin 0.01f) 0.27f
            normalized.Y |> should (equalWithin 0.01f) 0.53f
            normalized.Z |> should (equalWithin 0.01f) 0.80f

        [<Fact>]
        let ``coordinateSystem creates a coordinate system from a single vector`` () =
            let v2, v3 = Vector.coordinateSystem (Vector(1.0f, 0.0f, 0.0f))
            v2 |> should equal (Vector(0.0f, 0.0f, 1.0f))
            v3 |> should equal (Vector(0.0f, -1.0f, 0.0f))

        [<Fact>]
        let ``sphericalDirection converts spherical coordinates into a direction vector`` () =
            let theta = pi / 4.0f
            let phi = pi / 6.0f
            let result = Vector.sphericalDirection (sin theta) (cos theta) phi
            result.X |> should (equalWithin 0.01f) 0.61f
            result.Y |> should (equalWithin 0.01f) 0.35f
            result.Z |> should (equalWithin 0.01f) 0.71f

        [<Fact>]
        let ``sphericalDirection2 converts spherical coordinates into a direction vector with respect to a given coordinate frame`` () =
            let theta = pi / 4.0f
            let phi = pi / 6.0f
            let x = Vector(-1.0f, 0.0f, 0.0f)
            let y = Vector(0.0f, -1.0f, 0.0f)
            let z = Vector(0.0f, 0.0f, -1.0f)
            let result = Vector.sphericalDirection2 (sin theta) (cos theta) phi x y z
            result.X |> should (equalWithin 0.01f) -0.61f
            result.Y |> should (equalWithin 0.01f) -0.35f
            result.Z |> should (equalWithin 0.01f) -0.71f

        [<Theory>]
        [<InlineData(1.0f, 0.0f, 0.0f, 1.57f)>]
        [<InlineData(0.0f, 1.0f, 0.0f, 1.57f)>]
        [<InlineData(0.0f, 0.0f, 1.0f, 0.0f)>]
        let ``sphericalTheta converts a vector direction to a spherical theta angle`` x y z expectedValue =
            Vector.sphericalTheta (Vector(x, y, z)) |> should (equalWithin 0.01f) expectedValue

        [<Theory>]
        [<InlineData(1.0f, 0.0f, 0.0f, 0.0f)>]
        [<InlineData(0.0f, 1.0f, 0.0f, 1.57f)>]
        [<InlineData(0.0f, 0.0f, 1.0f, 0.0f)>]
        let ``sphericalPhi converts a vector direction to a spherical phi angle`` x y z expectedValue =
            Vector.sphericalPhi (Vector(x, y, z)) |> should (equalWithin 0.01f) expectedValue

        [<Theory>]
        [<InlineData(1.0f, 0.0f, 0.0f, 1.57f, 0.0f)>]
        [<InlineData(0.0f, 1.0f, 0.0f, 1.57f, 1.57f)>]
        [<InlineData(0.0f, 0.0f, 1.0f, 0.0f, 0.0f)>]
        let ``sphericalAngles converts a vector direction to spherical coordinates`` x y z expectedTheta expectedPhi =
            let theta, phi = Vector.sphericalAngles (Vector(x, y, z))
            theta |> should (equalWithin 0.01f) expectedTheta
            phi |> should (equalWithin 0.01f) expectedPhi

        [<Fact>]
        let ``zero should be zero`` () =
            Vector.zero |> should equal (Vector(0.0f, 0.0f, 0.0f))

        [<Fact>]
        let ``toNormal converts a Vector to a Normal`` () =
            Vector.toNormal vector |> should equal (Normal(1.0f, 2.0f, 3.0f))


namespace ``Geometry - Point``

    open Xunit
    open Xunit.Extensions
    open FsUnit.Xunit
    open Aether.Math
    open Aether.Geometry

    type ``Given a point`` () =
        let point = Point(1.0f, 2.0f, 3.0f)

        [<Fact>]
        let ``its properties return the correct values`` () =
            point.X |> should equal 1.0f
            point.Y |> should equal 2.0f
            point.Z |> should equal 3.0f

        [<Fact>]
        let ``its indexer returns the correct values`` () =
            point.[0] |> should equal 1.0f
            point.[1] |> should equal 2.0f
            point.[2] |> should equal 3.0f
            (fun () -> point.[3] |> ignore) |> should throw typeof<System.Exception>

        [<Fact>]
        let ``its arithmetic operators work`` () =
            point + Point(4.0f, 2.0f, -1.0f) |> should equal (Point(5.0f, 4.0f, 2.0f))
            point + Vector(4.0f, 2.0f, -1.0f) |> should equal (Point(5.0f, 4.0f, 2.0f))
            point - Point(4.0f, 2.0f, -1.0f) |> should equal (Vector(-3.0f, 0.0f, 4.0f))
            point - Vector(4.0f, 2.0f, -1.0f) |> should equal (Point(-3.0f, 0.0f, 4.0f))
            point * 2.0f |> should equal (Point(2.0f, 4.0f, 6.0f))
            2.0f * point |> should equal (Point(2.0f, 4.0f, 6.0f))
            point / 2.0f |> should equal (Point(0.5f, 1.0f, 1.5f))
            -point |> should equal (Point(-1.0f, -2.0f, -3.0f))

        [<Fact>]
        let ``its equality operator works`` () =
            point.Equals(Point(1.0f, 2.0f, 3.0f)) |> should be True
            point = Point(1.0f, 2.0f, 3.0f) |> should be True
            point.Equals(Point(1.1f, 2.0f, 3.0f)) |> should be False
            point = Point(1.1f, 2.0f, 3.0f) |> should be False

        [<Fact>]
        let ``distance calculates the distance between two points`` () =
            Point.distance point (Point(4.0f, 5.0f, 6.0f)) |> should (equalWithin 0.01f) 5.20f

        [<Fact>]
        let ``distanceSq calculates the square of the distance between two points`` () =
            Point.distanceSq point (Point(4.0f, 5.0f, 6.0f)) |> should equal 27.0f

        [<Fact>]
        let ``zero should be zero`` () =
            Point.zero |> should equal (Point(0.0f, 0.0f, 0.0f))

        [<Fact>]
        let ``toVector converts a Point to a Vector`` () =
            Point.toVector point |> should equal (Vector(1.0f, 2.0f, 3.0f))


namespace ``Geometry - Normal``

    open Xunit
    open Xunit.Extensions
    open FsUnit.Xunit
    open Aether.Geometry

    type ``Given a normal`` () =
        let normal = Normal(1.0f, 2.0f, 3.0f)

        [<Fact>]
        let ``its properties return the correct values`` () =
            normal.X |> should equal 1.0f
            normal.Y |> should equal 2.0f
            normal.Z |> should equal 3.0f

        [<Fact>]
        let ``its arithmetic operators work`` () =
            normal + Normal(4.0f, 2.0f, -1.0f) |> should equal (Normal(5.0f, 4.0f, 2.0f))
            normal - Normal(4.0f, 2.0f, -1.0f) |> should equal (Normal(-3.0f, 0.0f, 4.0f))
            normal * 2.0f |> should equal (Normal(2.0f, 4.0f, 6.0f))
            2.0f * normal |> should equal (Normal(2.0f, 4.0f, 6.0f))
            normal / 2.0f |> should equal (Normal(0.5f, 1.0f, 1.5f))

        [<Fact>]
        let ``its equality operator works`` () =
            normal.Equals(Normal(1.0f, 2.0f, 3.0f)) |> should be True
            normal = Normal(1.0f, 2.0f, 3.0f) |> should be True
            normal.Equals(Normal(1.1f, 2.0f, 3.0f)) |> should be False
            normal = Normal(1.1f, 2.0f, 3.0f) |> should be False

        [<Fact>]
        let ``lengthSq calculates the square of the length of a normal`` () =
            Normal.lengthSq normal |> should equal 14.0f

        [<Fact>]
        let ``length calculates the length of a normal`` () =
            Normal.length normal |> should (equalWithin 0.01f) 3.74f

        [<Fact>]
        let ``normalize normalizes a normal to unit length`` () =
            let normalized = Normal.normalize normal
            normalized.X |> should (equalWithin 0.01f) 0.27f
            normalized.Y |> should (equalWithin 0.01f) 0.53f
            normalized.Z |> should (equalWithin 0.01f) 0.80f


namespace ``Geometry - RaySegment``

    open Xunit
    open Xunit.Extensions
    open FsUnit.Xunit
    open Aether.Geometry

    type ``Given a ray segment`` () =
        let origin = Point(1.0f, 2.0f, 3.0f)
        let direction = Vector(0.0f, 0.0f, 1.0f)
        let ray = RaySegment(origin, direction, 0.1f, 10.0f, 0.5f)

        [<Fact>]
        let ``its properties return the correct values`` () =
            ray.Origin |> should equal origin
            ray.Direction |> should equal direction
            ray.MinT |> should equal 0.1f
            ray.MaxT |> should equal 10.0f
            ray.Time |> should equal 0.5f

        [<Fact>]
        let ``when evaluate is called, it calculates the position of the ray at the specified t`` () =
            RaySegment.evaluate 10.0f ray |> should equal (Point(1.0f, 2.0f, 13.0f))


namespace ``Geometry - BBox``

    open Xunit
    open Xunit.Extensions
    open FsUnit.Xunit
    open Aether.Geometry

    type ``Given a bounding box`` () =
        let min = Point(1.0f, 2.0f, 3.0f)
        let max = Point(4.0f, 5.0f, 6.0f)
        let bbox = BBox(min, max)

        [<Fact>]
        let ``its properties return the correct values`` () =
            bbox.Min |> should equal min
            bbox.Max |> should equal max

        [<Fact>]
        let ``unionBoxPoint creates a bounding box that contains the given point`` () =
            let point = Point(10.0f, 10.0f, 10.0f)
            let result = BBox.unionBoxPoint bbox point
            result.Min |> should equal min
            result.Max |> should equal point

        [<Fact>]
        let ``unionBoxBox creates a superset bounding box`` () =
            let point = Point(10.0f, 10.0f, 10.0f)
            let other = BBox(Point(5.0f, 5.0f, 5.0f), point)
            let result = BBox.unionBoxBox bbox other
            result.Min |> should equal min
            result.Max |> should equal point

        [<Theory>]
        [<InlineData(5.0f, 5.0f, 5.0f, false)>]
        [<InlineData(0.0f, 0.0f, 0.0f, true)>]
        let ``overlaps returns true if any part of the two bounding boxes overlap`` minX minY minZ expectedResult =
            let other = BBox(Point(minX, minY, minZ), Point(10.0f, 10.0f, 10.0f))
            BBox.overlaps bbox other |> should equal expectedResult

        [<Theory>]
        [<InlineData(5.0f, 5.0f, 5.0f, false)>]
        [<InlineData(2.0f, 3.0f, 4.0f, true)>]
        let ``inside returns true if the point is inside the bounding box`` x y z expectedResult =
            BBox.inside bbox (Point(x, y, z)) |> should equal expectedResult

        [<Fact>]
        let ``expand pads a bounding box by the specified amount`` () =
            let result = BBox.expand bbox 5.0f
            result.Min |> should equal (Point(-4.0f, -3.0f, -2.0f))
            result.Max |> should equal (Point(9.0f, 10.0f, 11.0f))

        [<Fact>]
        let ``diag returns the vector distance between min and max`` () =
            BBox.diag bbox |> should equal (Vector(3.0f, 3.0f, 3.0f))

        [<Fact>]
        let ``surfaceArea returns the surface area of the bounding box faces`` () =
            BBox.surfaceArea bbox |> should equal 54.0f

        [<Fact>]
        let ``volume returns the volume inside the bounding box`` () =
            BBox.volume bbox |> should equal 27.0f

        [<Fact>]
        let ``maximumExtent returns the largest axis of the bounding box`` () =
            BBox.maximumExtent bbox |> should equal 2

        [<Fact>]
        let ``boundingSphere returns a sphere that bounds the bounding box`` () =
            let center, radius = BBox.boundingSphere bbox
            center |> should equal (Point(2.5f, 3.5f, 4.5f))
            radius |> should (equalWithin 0.01f) 2.60f

    type ``Given two points`` () =
        let p1 = Point(4.0f, 5.0f, 6.0f)
        let p2 = Point(1.0f, 2.0f, 3.0f)

        [<Fact>]
        let ``fromPoints creates a bounding box`` () =
            let bbox = BBox.fromPoints p1 p2
            bbox.Min |> should equal p2
            bbox.Max |> should equal p1

    type ``Given one point`` () =
        let p1 = Point(4.0f, 5.0f, 6.0f)

        [<Fact>]
        let ``fromPoint creates a bounding box`` () =
            let bbox = BBox.fromPoint p1
            bbox.Min |> should equal p1
            bbox.Max |> should equal p1