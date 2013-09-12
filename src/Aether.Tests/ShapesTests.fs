module Aether.Tests.ShapesTests

open NUnit.Framework
open Nexus
open Nexus.Graphics.Transforms
open Aether.Math
open Aether.Shapes


module public PlaneTests =

    [<Test>]
    let ``can create instance``() =
        // Arrange.
        let point = Point3D(1.0f, 2.0f, 3.0f)
        let normal = Normal3D(0.0f, 1.0f, 0.0f)

        // Act.
        let plane = Plane(TranslateTransform (OffsetX = 1.0f, OffsetY = 2.0f, OffsetZ = 3.0f ), 
                          point, normal)

        // Assert.
        Assert.That(plane.Point, Is.EqualTo(point))
        Assert.That(plane.Normal, Is.EqualTo(normal))

    [<Test>]
    let ``TryIntersect returns true when ray hits``() =
        // Arrange.
        let plane = Plane(TranslateTransform(), 
                          Point3D(0.0f, 0.0f, 0.0f), 
                          Normal3D(0.0f, 1.0f, 0.0f))
        let ray = RaySegment3D(Point3D(0.0f, 4.0f, 0.0f), 
                               Vector3D.Down, 0.0f, 
                               System.Single.MaxValue, 
                               0.0f)
        
        // Act.
        let (result, tHit, dg) = plane.TryIntersect(ray)

        // Assert.
        Assert.That(result, Is.True)
        Assert.That(tHit, Is.EqualTo(4.0f))

    [<Test>]
    let ``TryIntersect returns false when ray misses``() =
        // Arrange.
        let plane = Plane(TranslateTransform(), 
                          Point3D(0.0f, 0.0f, 0.0f), 
                          Normal3D(0.0f, 1.0f, 0.0f))
        let ray = RaySegment3D(Point3D(0.0f, 1.0f, 0.0f), 
                               Vector3D.Forward, 0.0f, 
                               System.Single.MaxValue, 
                               0.0f)
        
        // Act.
        let (result, tHit, dg) = plane.TryIntersect(ray)

        // Assert.
        Assert.That(result, Is.False)


module public SphereTests =

    [<Test>]
    let ``can create instance``() =
        // Act.
        let sphere = Sphere(TranslateTransform (OffsetX = 1.0f, OffsetY = 2.0f, OffsetZ = 3.0f ), 
                            10.0f)

        // Assert.
        Assert.That(sphere.Radius, Is.EqualTo(10.0f))

    [<Test>]
    let ``TryIntersect returns true when ray hits``() =
        // Arrange.
        let sphere = Sphere(TranslateTransform (OffsetZ = 3.0f), 10.0f) 
        let ray = RaySegment3D(Point3D(0.0f, 0.0f, 20.0f), 
                               Vector3D.Forward, 0.0f, 
                               System.Single.MaxValue, 
                               0.0f)
        
        // Act.
        let (result, tHit, _) = sphere.TryIntersect(ray)

        // Assert.
        Assert.That(result, Is.True)
        Assert.That(tHit, Is.EqualTo(7.0f))

    [<Test>]
    let ``TryIntersect returns false when ray misses``() =
        // Arrange.
        let sphere = Sphere(TranslateTransform (OffsetZ = 3.0f), 10.0f) 
        let ray = RaySegment3D(Point3D(20.0f, 0.0f, 20.0f), 
                               Vector3D.Forward, 0.0f, 
                               System.Single.MaxValue, 
                               0.0f)
        
        // Act.
        let (result, _, _) = sphere.TryIntersect(ray)

        // Assert.
        Assert.That(result, Is.False)