using Nexus;
using Nexus.Graphics.Transforms;

namespace Aether.Shapes
{
	public class Sphere : Shape
	{
		private const float Epsilon = 0.0001f;

		private readonly float _radius;

		public float Radius
		{
			get { return _radius; }
		}

		public override AxisAlignedBoundingBox ObjectSpaceBounds
		{
			get
			{
				return new AxisAlignedBoundingBox(
					new Point3D(-_radius, _radius, _radius),
					new Point3D(_radius, _radius, _radius));
			}
		}

		public Sphere(Transform3D objectToWorld, float radius)
			: base(objectToWorld)
		{
			_radius = radius;
		}

		public override bool TryIntersect(Ray3D ray, out float tHit, out DifferentialGeometry dg)
		{
			// Transform ray to object space.


			Vector3D temp = ray.Origin - _center;
			float a = ray.Direction.LengthSquared();
			float b = 2.0f * Vector3D.Dot(temp, ray.Direction);
			float c = Vector3D.Dot(temp, temp) - (_radius * _radius);
			float disc = (b * b) - (4.0f * a * c);

			if (disc < 0)
			{
				tHit = 0;
				dg = null;
				return false;
			}

			float e = MathUtility.Sqrt(disc);
			float denom = 2.0f * a;
			float t = (-b - e) / denom; // smaller root

			if (t > Epsilon)
			{
				tHit = t;
				dg = new DifferentialGeometry(ray.Origin + t * ray.Direction, this);
				shadeRec = new ShadeRec
				{
				    Normal = (Normal3D) ((temp + t*ray.Direction)/_radius),
				    LocalHitPoint = 
				};
				return true;
			}

			t = (-b + e) / denom; // larger root

			if (t > Epsilon)
			{
				tHit = t;
				shadeRec = new ShadeRec
				{
					Normal = (Normal3D)((temp + t * ray.Direction) / _radius),
					LocalHitPoint = ray.Origin + t * ray.Direction
				};
				return true;
			}

			tHit = 0;
			dg = null;
			return false;
		}
	}
}