using Aether.Primitives;
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
			float tMax = float.MaxValue;
			Intersection intersection;
			if (!scene.TryIntersect(ray, float.MinValue, ref tMax, out intersection))
			{
				// Handle ray with no intersection
				// TODO: Iterate through lights to see what they contribute to this ray
				return ColorsF.Black;
			}

			return ColorsF.Blue;
		}
	}
}