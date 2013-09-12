namespace Aether.Integrators

open Nexus
open Nexus.Graphics.Colors
open Aether
open Aether.Lights
open Aether.Math
open Aether.Primitives
open Aether.Sampling


type IIntegratableScene =
    inherit IIntersectable
    abstract Lights : seq<Light>
    abstract TryIntersect : RaySegment3D -> (bool * option<Intersection>)


[<AbstractClass>]
type Integrator() =
    abstract Li : IIntegratableScene -> RaySegment3D -> Sample -> ColorF

    abstract Preprocess : IIntegratableScene -> unit
    default this.Preprocess scene = ()


[<AbstractClass>]
type SurfaceIntegrator() =
    inherit Integrator()


type WhittedIntegrator(maxDepth) =
    inherit SurfaceIntegrator()

    override this.Li scene ray sample =
        // Search for ray-primitive intersection
        let (intersects, intersection) = scene.TryIntersect(ray)
        if not(intersects) then
            // Handle ray with no intersection
            // TODO: Iterate through lights to see what they contribute to this ray
            ColorsF.Black
        else
            // TODO: Initialize alpha for ray hit.

            // Compute emitted and reflected light at ray intersection point.
            // Evaluate BSDF at hit point.
            let bsdf = (Option.get intersection).GetBsdf(ray)

            // Initialize common variables for Whitted integrator.
            let p = bsdf.ShadingGeometry.Point
            let n = bsdf.ShadingGeometry.Normal
            let wo = -ray.Direction

            // TODO: Compute emitted light if ray hit an area light source.

            // Add contribution of each light source.
            let mutable result = ColorF()
            for light in scene.Lights do
                let (li, directionToLight, visibilityTester) = light.Evaluate(p)

                if not(li.IsEmpty()) then // Early exit for no light
                    let f = bsdf.Evaluate(wo, directionToLight)
                    if not(f.IsEmpty()) && visibilityTester.Unoccluded(scene) then
                        result <- result + f * li * Vector3D.AbsDot(directionToLight, n)
                            //* visibilityTester.Transmittance(scene);

            // TODO --_rayDepth; 
            result