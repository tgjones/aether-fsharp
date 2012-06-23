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

		public override bool TryIntersect(Ray3D ray, out Intersection intersection)
		{
			float tHit;
			DifferentialGeometry dg;
			if (!_shape.TryIntersect(ray, out tHit, out dg))
			{
				intersection = null;
				return false;
			}

			intersection = new Intersection(this, dg);
			return true;
		}

		public override bool Intersects(Ray3D ray)
		{
			return _shape.Intersects(ray);
		}
	}
}