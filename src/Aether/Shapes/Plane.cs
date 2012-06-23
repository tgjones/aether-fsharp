using Nexus;
using Nexus.Graphics.Transforms;

namespace Aether.Shapes
{
	public class Plane : Shape
	{
		private const float Epsilon = 0.0001f;

		private readonly Point3D _point;
		private readonly Normal3D _normal;

		public Point3D Point
		{
			get { return _point; }
		}

		public Normal3D Normal
		{
			get { return _normal; }
		}

		public override AxisAlignedBoundingBox ObjectSpaceBounds
		{
			get { throw new System.NotImplementedException(); }
		}

		public Plane(Transform3D objectToWorld, Point3D point, Normal3D normal)
			: base(objectToWorld)
		{
			_point = point;
			_normal = normal;
		}

		public override bool TryIntersect(Nexus.Ray3D ray, float tMin, float tMax, out float tHit, out DifferentialGeometry dg)
		{
			throw new System.NotImplementedException();
		}

		//public override bool TryIntersect(Ray3D ray, out float tMin, out DifferentialGeometry dg)
		//{
		//    float t = Vector3D.Dot((_point - ray.Origin), _normal) / Vector3D.Dot(ray.Direction, _normal);

		//    if (t > Epsilon)
		//    {
		//        tMin = t;
		//        shadeRec = new ShadeRec();
		//        shadeRec.Normal = _normal;
		//        shadeRec.LocalHitPoint = ray.Origin + (t * ray.Direction);
		//        return true;
		//    }

		//    tMin = 0;
		//    shadeRec = null;
		//    return false;
		//}
	}
}