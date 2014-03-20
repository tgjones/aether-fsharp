namespace Aether.Integrators

open Aether
open Aether.Math
open Aether.Geometry
open Aether.Lights
open Aether.Primitives
open Aether.Sampling


type IIntegratableScene =
  inherit IIntersectable
  abstract Lights : seq<Light>
  abstract TryIntersect : RaySegment -> Intersection option


[<AbstractClass>]
type Integrator() =
  abstract Li : IIntegratableScene -> RaySegment -> Sample -> Spectrum

  abstract Preprocess : IIntegratableScene -> unit
  default this.Preprocess scene = ()


[<AbstractClass>]
type SurfaceIntegrator() =
  inherit Integrator()


type WhittedIntegrator(maxDepth) =
  inherit SurfaceIntegrator()

  override this.Li scene ray sample =
    // Search for ray-primitive intersection
    match scene.TryIntersect(ray) with
    | Some(intersection) ->
      // TODO: Initialize alpha for ray hit.

      // Compute emitted and reflected light at ray intersection point.
      // Evaluate BSDF at hit point.
      let bsdf = intersection.GetBsdf(ray)

      // Initialize common variables for Whitted integrator.
      let p = bsdf.ShadingGeometry.Point
      let n = bsdf.ShadingGeometry.Normal
      let wo = -ray.Direction

      // TODO: Compute emitted light if ray hit an area light source.

      // Add contribution of each light source.
      let result = Spectrum.Black()
      for light in scene.Lights do
          let (li, directionToLight, visibilityTester) = light.Evaluate p intersection.RayEpsilon ray.Time

          if not(li.IsBlack()) then // Early exit for no light
              let f = bsdf.Evaluate(wo, directionToLight)
              if not(f.IsBlack()) && visibilityTester.Unoccluded(scene) then
                  result.Add(f * li * Vector.AbsDot(directionToLight, n))
                      //* visibilityTester.Transmittance(scene);

      // TODO --_rayDepth; 
      result

    | None ->
      // Handle ray with no intersection
      // TODO: Iterate through lights to see what they contribute to this ray
      Spectrum.Black()


type LightStrategy =
    | SampleAllUniform = 0
    | SampleOneUniform = 1