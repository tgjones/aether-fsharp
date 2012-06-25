using Nexus;
using Nexus.Graphics.Transforms;
using Aether.Shapes;
using Aether.Materials.Reflectance;

namespace Aether.Primitives
{
	public abstract class Primitive
	{
		public abstract bool TryIntersect(RaySegment3D ray, out Intersection intersection);
		public abstract bool Intersects(RaySegment3D ray);

        public abstract Bsdf GetBsdf(DifferentialGeometry dg, Transform3D worldToObject);
	}
}