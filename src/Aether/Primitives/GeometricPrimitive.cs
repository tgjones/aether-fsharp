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

		public override bool TryIntersect(Ray3D ray, out float tMin, out ShadeRec shadeRec)
		{
			return _shape.Hit(ray, out tMin, out shadeRec);
		}

		public override bool Intersects(Ray3D ray)
		{
			// TODO: Allow shape to optimise this.
			float tMin;
			ShadeRec shadeRec;
			return TryIntersect(ray, out tMin, out shadeRec);
		}
	}
}