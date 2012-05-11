using Aether.Cameras;
using Aether.Integrators;
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
		private readonly Shape _shape;

		public Scene(Camera camera, SurfaceIntegrator surfaceIntegrator,
			Sampler sampler, Shape shape)
		{
			_camera = camera;
			_surfaceIntegrator = surfaceIntegrator;
			_sampler = sampler;
			_shape = shape;
		}

		public bool TryIntersect(Ray3D ray, out float tMin, out ShadeRec shadeRec)
		{
			return _shape.Hit(ray, out tMin, out shadeRec);
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