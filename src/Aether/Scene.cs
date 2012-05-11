using Aether.Cameras;
using Aether.Sampling;
using Aether.Shapes;
using Nexus;

namespace Aether
{
	public class Scene
	{
		private readonly Camera _camera;
		private readonly Sampler _sampler;
		private readonly Shape _shape;

		public Scene(Camera camera, Sampler sampler, Shape shape)
		{
			_camera = camera;
			_sampler = sampler;
			_shape = shape;
		}

		public void Render()
		{
			Sample sample;
			while (_sampler.GetNextSample(out sample))
			{
				var ray = _camera.GenerateRay(sample);
				var color = Li(ray, sample);
				_camera.Film.AddSample(sample, color);
			}
		}

		private ColorF Li(Ray3D ray, Sample sample)
		{
			// TODO: Use Integrator to actually trace ray.
			float tMin;
			ShadeRec sr;
			if (_shape.Hit(ray, out tMin, out sr))
			{
				return new ColorF(1, 0, 0);
			}
			return ColorsF.Blue;
		}
	}
}