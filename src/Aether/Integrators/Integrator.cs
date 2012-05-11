using Aether.Sampling;
using Nexus;

namespace Aether.Integrators
{
	public abstract class Integrator
	{
		public abstract ColorF Li(Scene scene, Ray3D ray, Sample sample);

		public virtual void Preprocess(Scene scene)
		{
		}
	}
}