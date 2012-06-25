using Aether.Materials.Reflectance;
using Aether.Shapes;
using Nexus;
using Nexus.Graphics.Transforms;

namespace Aether.Primitives
{
	public class Intersection
	{
        private readonly Primitive _primitive;
        private readonly DifferentialGeometry _dg;
        private readonly Transform3D _worldToObject;

		public Intersection(Primitive primitive, DifferentialGeometry dg, Transform3D worldToObject)
		{
            _primitive = primitive;
            _dg = dg;
		}

		public Bsdf GetBsdf(RaySegment3D ray)
		{
            _dg.ComputeDifferentials(ray);
            return _primitive.GetBsdf(_dg, _worldToObject);
		}
	}
}