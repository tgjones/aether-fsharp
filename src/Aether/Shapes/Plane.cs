using Nexus;

namespace Aether.Shapes
{
	public class Plane : Shape
	{
		private const float Epsilon = 0.0001f;

		private readonly Point3D _point;
		private readonly Normal3D _normal;

		public Plane(Point3D point, Normal3D normal)
		{
			_point = point;
			_normal = normal;
		}

		public override bool Hit(Ray3D ray, out float tMin, out ShadeRec shadeRec)
		{
			float t = Vector3D.Dot((_point - ray.Origin), _normal) / Vector3D.Dot(ray.Direction, _normal);

			if (t > Epsilon)
			{
				tMin = t;
				shadeRec = new ShadeRec();
				shadeRec.Normal = _normal;
				shadeRec.LocalHitPoint = ray.Origin + (t * ray.Direction);
				return true;
			}

			tMin = 0;
			shadeRec = null;
			return false;
		}
	}
}