namespace Aether.Lights

open Aether
open Aether.Math
open Aether.Geometry
open Aether.Transforms
open Aether.Parsing


type IIntersectable =
    abstract Intersects : RaySegment -> bool


type VisibilityTester(ray : RaySegment) =
    member this.Ray = ray

    new(p1 : Point, epsilon1, p2 : Point, epsilon2, time) =
        let distance = Point.Distance(p1, p2)
        let ray = RaySegment(p1, p2 - p1, epsilon1,
                             distance * (1.0f - epsilon2),
                             time)
        VisibilityTester(ray)

    new(p, epsilon, w, time) =
        let ray = RaySegment(p, w, epsilon, infinityf, time)
        VisibilityTester(ray)

    member this.Unoccluded(scene : IIntersectable) =
        not(scene.Intersects ray)


type LightSampleOffsets(count, sample) =
    


type LightSample(uPos0, uPos1, uComponent) =

    new(sample, offsets, num) = LightSample()
    new(rng : System.Random) = LightSample(rng.NextSingle(), rng.NextSingle(), rng.NextSingle())
    


[<AbstractClass>]
type Light(lightToWorld : Transform) =
    /// Calculates the radiance arriving at the specified world-space point due to this light. 
    abstract SampleL : Point -> single -> LightSample -> single -> (Spectrum * Vector * single * VisibilityTester)

//    abstract Power : IIntersectable -> ColorF


type PointLight(lightToWorld : Transform, intensity : Spectrum) =
    inherit Light(lightToWorld)

    let position = lightToWorld |>> Point.Zero

    override this.Evaluate point epsilon time =
        let vectorToLight = position - point
        let directionToLight = Vector.Normalize vectorToLight
        let visibilityTester = VisibilityTester(point, epsilon, position, 0.0f, time)
        let result = intensity / (vectorToLight.LengthSquared())
        (result, directionToLight, visibilityTester)


type DistantLight(lightToWorld : Transform, radiance, direction : Vector) =
    inherit Light(lightToWorld)

    let direction = lightToWorld |>> direction |> Vector.Normalize

    override this.Evaluate point epsilon time =
        let visibilityTester = VisibilityTester(point, epsilon, direction, time)
        (radiance, direction, visibilityTester)