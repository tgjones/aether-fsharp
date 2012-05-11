using Nexus;

namespace Aether.Primitives
{
	public abstract class Primitive
	{
		public abstract bool TryIntersect(Ray3D ray, out float tMin, out ShadeRec shadeRec);
		public abstract bool Intersects(Ray3D ray);
	}
}