namespace Aether.Lights

open Aether
open Aether.Geometry
open Aether.Transforms
open Aether.Math


type IIntersectable =
    abstract Intersects : RaySegment -> bool


type VisibilityTester(ray : RaySegment) =
    member this.Ray = ray

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module VisibilityTester =

    let initPoints (p1 : Point) epsilon1 (p2 : Point) epsilon2 time =
        let distance = Point.distance p1 p2
        let ray = RaySegment(p1, p2 - p1, epsilon1,
                   distance * (1.0f - epsilon2),
                   time)
        VisibilityTester(ray)

    let initPointVector p epsilon w time =
        let ray = RaySegment(p, w, epsilon, infinityf, time)
        VisibilityTester(ray)

    let unoccluded (scene : IIntersectable) (tester : VisibilityTester) =
        not(scene.Intersects tester.Ray)


[<AbstractClass>]
type Light(lightToWorld : Transform) =
    /// Calculates the radiance arriving at the specified world-space point due to this light. 
    abstract Evaluate : Point -> single -> single -> (Spectrum * Vector * VisibilityTester)

//    abstract Power : IIntersectable -> ColorF


type PointLight(lightToWorld, intensity : Spectrum) =
    inherit Light(lightToWorld)

    let position = lightToWorld |>> Point.zero

    override this.Evaluate point epsilon time =
        let vectorToLight = position - point
        let directionToLight = Vector.normalize vectorToLight
        let visibilityTester = VisibilityTester.initPoints point epsilon position 0.0f time
        let result = intensity / (Vector.lengthSq vectorToLight)
        (result, directionToLight, visibilityTester)


type DistantLight(lightToWorld, radiance, direction) =
    inherit Light(lightToWorld)

    let direction = lightToWorld |>> direction |> Vector.normalize

    override this.Evaluate point epsilon time =
        let visibilityTester = VisibilityTester.initPointVector point epsilon direction time
        (radiance, direction, visibilityTester)