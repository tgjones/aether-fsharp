using Aether.Sampling;
using Nexus;

namespace Aether.Integrators
{
	public class WhittedIntegrator : SurfaceIntegrator
	{
		private readonly int _maxDepth;
		private int _rayDepth;
		
		public WhittedIntegrator(int maxDepth)
		{
			_maxDepth = maxDepth;
		}

		public override ColorF Li(Scene scene, Ray3D ray, Sample sample)
		{
			// Search for ray-primitive intersection
			float tMin;
			ShadeRec shadeRec;
			if (!scene.TryIntersect(ray, out tMin, out shadeRec))
			{
				// Handle ray with no intersection
				// TODO: Iterate through lights to see what they contribute to this ray
				return ColorsF.Black;
			}

			return ColorsF.Blue;
		}
	}
}