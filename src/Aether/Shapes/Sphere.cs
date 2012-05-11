using Nexus;

namespace Aether.Shapes
{
	public class Sphere : Shape
	{
		private const float Epsilon = 0.0001f;

		private readonly Point3D _center;
		private readonly float _radius;

		public Sphere(Point3D center, float radius)
		{
			_center = center;
			_radius = radius;
		}

		public override bool Hit(Ray3D ray, out float tMin, out ShadeRec shadeRec)
		{
			Vector3D temp = ray.Origin - _center;
			float a = ray.Direction.LengthSquared();
			float b = 2.0f * Vector3D.Dot(temp, ray.Direction);
			float c = Vector3D.Dot(temp, temp) - (_radius * _radius);
			float disc = (b * b) - (4.0f * a * c);

			if (disc < 0)
			{
				tMin = 0;
				shadeRec = null;
				return false;
			}

			float e = MathUtility.Sqrt(disc);
			float denom = 2.0f * a;
			float t = (-b - e) / denom; // smaller root

			if (t > Epsilon)
			{
				tMin = t;
				shadeRec = new ShadeRec
				{
				    Normal = (Normal3D) ((temp + t*ray.Direction)/_radius),
				    LocalHitPoint = ray.Origin + t*ray.Direction
				};
				return true;
			}

			t = (-b + e) / denom; // larger root

			if (t > Epsilon)
			{
				tMin = t;
				shadeRec = new ShadeRec
				{
					Normal = (Normal3D)((temp + t * ray.Direction) / _radius),
					LocalHitPoint = ray.Origin + t * ray.Direction
				};
				return true;
			}

			tMin = 0;
			shadeRec = null;
			return false;
		}
	}
}