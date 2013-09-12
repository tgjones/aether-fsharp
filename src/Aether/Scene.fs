namespace Aether

open Aether.Lights
open Aether.Shapes
open Aether.Primitives
open Aether.Sampling
open Aether.Integrators
open Aether.Cameras

type Scene(camera : Camera, 
           surfaceIntegrator : SurfaceIntegrator,
           sampler : Sampler,
           primitive : Primitive, 
           lights) =

    interface IIntersectable with
        member this.Intersects ray = primitive.Intersects ray

    interface IIntegratableScene with
        member this.Lights = lights
        member this.TryIntersect ray = primitive.TryIntersect ray
        
    member this.Render () =
        for sample in sampler.GetSamples() do
            let ray = camera.GenerateRay sample

            let li ray sample = surfaceIntegrator.Li this ray sample

            // Evaluate radiance along camera ray.
            let color = li ray sample

            camera.Film.AddSample sample color