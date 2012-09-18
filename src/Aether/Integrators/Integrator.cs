using Aether.Sampling;
using Nexus.Graphics.Colors;

namespace Aether.Integrators
{
	public abstract class Integrator
	{
		public abstract ColorF Li(Scene scene, RaySegment3D ray, Sample sample);

		public virtual void Preprocess(Scene scene)
		{
		}
	}
}