using Aether.Cameras;
using Aether.Integrators;
using Aether.Primitives;
using Aether.Sampling;
using Aether.Shapes;
using Nexus;

namespace Aether
{
	public class Scene
	{
		private readonly Camera _camera;
		private readonly SurfaceIntegrator _surfaceIntegrator;
		private readonly Sampler _sampler;
		private readonly Primitive _primitive;

		public Scene(Camera camera, SurfaceIntegrator surfaceIntegrator,
			Sampler sampler, Primitive primitive)
		{
			_camera = camera;
			_surfaceIntegrator = surfaceIntegrator;
			_sampler = sampler;
			_primitive = primitive;
		}

		public bool TryIntersect(Ray3D ray, float tMin, ref float tMax, out Intersection intersection)
		{
			return _primitive.TryIntersect(ray, tMin, ref tMax, out intersection);
		}

		public void Render()
		{
			Sample sample;
			while (_sampler.GetNextSample(out sample))
			{
				var ray = _camera.GenerateRay(sample);

				// Evaluate radiance along camera ray.
				var color = Li(ray, sample);

				_camera.Film.AddSample(sample, color);
			}
		}

		private ColorF Li(Ray3D ray, Sample sample)
		{
			return _surfaceIntegrator.Li(this, ray, sample);
		}
	}
}