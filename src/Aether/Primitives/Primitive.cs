using Nexus;

namespace Aether.Primitives
{
	public abstract class Primitive
	{
		public abstract bool TryIntersect(Ray3D ray, out Intersection intersection);
		public abstract bool Intersects(Ray3D ray);
	}
}