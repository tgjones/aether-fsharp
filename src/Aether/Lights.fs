namespace Aether.Lights

open Aether
open Aether.Math
open Nexus
open Nexus.Graphics.Colors
open Nexus.Graphics.Transforms
open Nexus.Objects3D


type IIntersectable =
    abstract Intersects : RaySegment3D -> bool


type VisibilityTester(ray : RaySegment3D) =
    static member Create (p, v) =
        new VisibilityTester(new RaySegment3D(p, v, RaySegment3D.Epsilon))

    static member Create (p1, p2) =
        new VisibilityTester(
            new RaySegment3D(p1, p2 - p1, RaySegment3D.Epsilon,
                             1.0f - RaySegment3D.Epsilon))

    member this.Unoccluded (scene : IIntersectable) =
        not(scene.Intersects ray)


[<AbstractClass>]
type Light(transform : Transform3D) =
    /// <summary>
    /// Calculates the radiance arriving at the specified world-space point due to this light. 
    /// </summary>
    /// <param name="point"></param>
    /// <param name="directionToLight"></param>
    /// <returns></returns>
    abstract member Evaluate: Point3D -> (ColorF * Vector3D * VisibilityTester)


type PointLight(transform, intensity : ColorF) =
    inherit Light(transform)

    let position = transform.Transform(Point3D.Zero)

    override this.Evaluate point =
        let vectorToLight = position - point
        let directionToLight = Vector3D.Normalize(vectorToLight)
        let visibilityTester = VisibilityTester.Create (point, position)
        let result = intensity / vectorToLight.LengthSquared()
        (result, directionToLight, visibilityTester)


type DirectionalLight(transform, direction : Vector3D, radiance) =
    inherit Light(transform)

    let direction = Vector3D.Normalize(transform.Transform(direction))

    override this.Evaluate point =
        let visibilityTester = VisibilityTester.Create (point, direction)
        (radiance, direction, visibilityTester)