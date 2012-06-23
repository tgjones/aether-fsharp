using Aether.Materials;
using Aether.Shapes;
using Nexus;

namespace Aether.Primitives
{
	public class GeometricPrimitive : Primitive
	{
		private readonly Shape _shape;

		public GeometricPrimitive(Shape shape, Material material)
		{
			_shape = shape;
		}

		public override bool TryIntersect(Ray3D ray, float tMin, ref float tMax, out Intersection intersection)
		{
			float tHit;
			DifferentialGeometry dg;
			if (!_shape.TryIntersect(ray, tMin, tMax, out tHit, out dg))
			{
				intersection = null;
				return false;
			}

			intersection = new Intersection(this, dg);
			tMax = tHit;
			return true;
		}

		public override bool Intersects(Ray3D ray, float tMin, float tMax)
		{
			return _shape.Intersects(ray, tMin, tMax);
		}
	}
}