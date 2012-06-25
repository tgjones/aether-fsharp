using Aether.Materials;
using Aether.Shapes;
using Nexus;
using Aether.Materials.Reflectance;
using Nexus.Graphics.Transforms;

namespace Aether.Primitives
{
	public class GeometricPrimitive : Primitive
	{
		private readonly Shape _shape;
        private readonly Material _material;

		public GeometricPrimitive(Shape shape, Material material)
		{
			_shape = shape;
            _material = material;
		}

		public override bool TryIntersect(RaySegment3D ray, out Intersection intersection)
		{
			float tHit;
			DifferentialGeometry dg;
			if (!_shape.TryIntersect(ray, out tHit, out dg))
			{
				intersection = null;
				return false;
			}

			intersection = new Intersection(this, dg, _shape.WorldToObject);
            ray.MaxT = tHit;
			return true;
		}

		public override bool Intersects(RaySegment3D ray)
		{
			return _shape.Intersects(ray);
		}

        public override Bsdf GetBsdf(DifferentialGeometry dg, Transform3D worldToObject)
        {
            // TODO: Allow shape to use a different geometry for shading.
            return _material.GetBsdf(dg);
        }
	}
}