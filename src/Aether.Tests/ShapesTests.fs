namespace ``Shapes - Plane shape``
    open Xunit
    open FsUnit.Xunit
    open Aether.Math
    open Aether.Geometry
    open Aether.Transforms
    open Aether.Shapes

    type ``Given valid input data`` () =
        let transform = Transform.Translate (Vector(1.0f, 2.0f, 3.0f))
        let point = Point(1.0f, 2.0f, 3.0f)
        let normal = Normal(0.0f, 1.0f, 0.0f)

        [<Fact>]
        let ``when constructor is called, it returns a valid instance`` () =
            let plane = Plane(transform, false, point, normal)
            plane.Point |> should equal point
            plane.Normal |> should equal normal

    type ``Given a ray that hits the plane`` () =
        let transform = Transform.Translate Vector.Zero
        let plane = Plane(transform, false,
                          Point(0.0f, 0.0f, 0.0f), 
                          Normal(0.0f, 1.0f, 0.0f))
        let ray = RaySegment(Point(0.0f, 4.0f, 0.0f), 
                             Vector(0.0f, -1.0f, 0.0f),
                             0.0f, infinityf)

        [<Fact>]
        let ``when intersection test is run, it returns true`` () =
            let result = plane.TryIntersect(ray)
            result |> Option.isSome |> should be True
            let tHit, rayEpsilon, dg = Option.get result
            tHit |> should equal 4.0f

    type ``Given a ray that misses the plane`` () =
        let transform = Transform.Translate Vector.Zero
        let plane = Plane(transform, false, 
                          Point(0.0f, 0.0f, 0.0f), 
                          Normal(0.0f, 1.0f, 0.0f))
        let ray = RaySegment(Point(0.0f, 1.0f, 0.0f), 
                             Vector(0.0f, 1.0f, 0.0f),
                             0.0f, infinityf)

        [<Fact>]
        let ``when intersection test is run, it returns false`` () =
            plane.TryIntersect(ray) |> Option.isNone |> should be True

namespace ``Shapes - Sphere shape``
    open Xunit
    open FsUnit.Xunit
    open Aether.Math
    open Aether.Geometry
    open Aether.Transforms
    open Aether.Shapes

    type ``Given valid input data`` () =
        let transform = Transform.Translate (Vector(1.0f, 2.0f, 3.0f))
        let radius = 10.0f

        [<Fact>]
        let ``when constructor is called, it returns a valid instance`` () =
            let sphere = Sphere(transform, false, radius, -radius, radius, pi * 2.0f)
            sphere.Radius |> should equal radius

    type ``Given a ray that hits the sphere`` () =
        let transform = Transform.Translate (Vector(0.0f, 0.0f, 0.0f))
        let radius = 10.0f
        let sphere = Sphere(transform, false, radius, -radius, radius, pi * 2.0f)
        let ray = RaySegment(Point(0.0f, 0.0f, -20.0f), 
                             Vector(0.0f, 0.0f, 1.0f),
                             0.0f, infinityf)

        [<Fact>]
        let ``when intersection test is run, it returns a value`` () =
            let result = sphere.TryIntersect(ray)
            result |> Option.isSome |> should be True
            let tHit, rayEpsilon, dg = Option.get result
            tHit |> should equal 10.0f

    type ``Given a ray that misses the sphere`` () =
        let transform = Transform.Translate (Vector(0.0f, 0.0f, 3.0f))
        let radius = 10.0f
        let sphere = Sphere(transform, false, radius, -radius, radius, pi * 2.0f)
        let ray = RaySegment(Point(20.0f, 0.0f, 20.0f), 
                             Vector(0.0f, 1.0f, 0.0f),
                             0.0f, infinityf)

        [<Fact>]
        let ``when intersection test is run, it returns None`` () =
            sphere.TryIntersect(ray) |> Option.isNone |> should be True