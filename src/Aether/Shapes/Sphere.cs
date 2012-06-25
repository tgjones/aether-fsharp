using Aether.Util;
using Nexus;
using Nexus.Graphics.Transforms;

namespace Aether.Shapes
{
	public class Sphere : Shape
	{
		private const float Epsilon = 0.0001f;
		private const float PhiMax = MathUtility.TWO_PI;
		private const float ThetaMin = MathUtility.PI;
		private const float ThetaMax = MathUtility.TWO_PI;

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

		public override bool TryIntersect(RaySegment3D ray, out float tHit, out DifferentialGeometry dg)
		{
			// Initialize output.
			tHit = float.MinValue;
			dg = null;

			// Transform ray to object space.
			ray = WorldToObject.Transform(ray);

			// Compute quadratic sphre coefficients.
			var origin = (Vector3D) ray.Origin;
			float a = ray.Direction.LengthSquared();
			float b = 2.0f * Vector3D.Dot(origin, ray.Direction);
			float c = Vector3D.Dot(origin, origin) - (_radius * _radius);

			// Solve quadratic equation
			float t0, t1;
			if (!Quadratic.Solve(a, b, c, out t0, out t1))
				return false;

			// Compute intersection distance along ray.
			if (t0 > ray.MaxT || t1 < ray.MinT)
				return false;
			float tHitTemp = t0;
			if (t0 < ray.MinT) // Is first intersection before the range we're interested in?
			{
				tHitTemp = t1;
				if (tHitTemp > ray.MaxT) // Is second intersection after the range we're interested in?
					return false;
			}

			// Compute sphere hit position and phi
			Point3D pHit = ray.Evaluate(tHitTemp);
			float phi = MathUtility.Atan2(pHit.Y, pHit.X);
			if (phi < 0)
				phi += MathUtility.TWO_PI;
			
			// Find parametric representation of sphere hit.
			float u = phi / PhiMax;
			float theta = MathUtility.Acos(MathUtility.Clamp(pHit.Z / _radius, -1, 1));
			float v = (theta - ThetaMin) / (ThetaMax - ThetaMin);

			// Compute sphere dpdu and dpdv.
			float cosPhi, sinPhi;
			Vector3D dpDu, dpDv;
			float zRadius = MathUtility.Sqrt(pHit.X * pHit.X + pHit.Y * pHit.Y);
			if (zRadius == 0.0f)
			{
				// Handle hit at degenerate parameterization point.
				cosPhi = 0;
				sinPhi = 1;
				dpDv = (ThetaMax - ThetaMin) * new Vector3D(pHit.Z * cosPhi, pHit.Z * sinPhi, -Radius * MathUtility.Sin(theta));
				dpDu = Vector3D.Cross(dpDv, (Vector3D) pHit);
			}
			else
			{
				float inverseZRadius = 1 / zRadius;
				cosPhi = pHit.X * inverseZRadius;
				sinPhi = pHit.Y * inverseZRadius;
				dpDu = new Vector3D(-PhiMax * pHit.Y, PhiMax * pHit.X, 0);
				// TODO: The following line is identical to the branch above.
				dpDv = (ThetaMax - ThetaMin) * new Vector3D(pHit.Z * cosPhi, pHit.Z * sinPhi, -Radius * MathUtility.Sin(theta));
			}

			// Compute sphere dndu and dndv.
			Vector3D d2Pduu = -PhiMax * PhiMax * new Vector3D(pHit.X, pHit.Y, 0);
			Vector3D d2Pduv = (ThetaMax - ThetaMin) * pHit.Z * PhiMax * new Vector3D(-sinPhi, cosPhi, 0);
			Vector3D d2Pdvv = -(ThetaMax - ThetaMin) * (ThetaMax - ThetaMin) * (Vector3D) pHit;

			// Compute coefficients for fundamental forms.
			float E = dpDu.LengthSquared();
			float F = Vector3D.Dot(dpDu, dpDv);
			float G = dpDv.LengthSquared();
			Vector3D n = Vector3D.Normalize(Vector3D.Cross(dpDu, dpDv));
			float e = Vector3D.Dot(n, d2Pduu);
			float f = Vector3D.Dot(n, d2Pduv);
			float g = Vector3D.Dot(n, d2Pdvv);

			// Compute dndu and dndv from fundamental form coefficients.
			float invEGF2 = 1 / (E * G - F * F);
			Vector3D dnDu = (f * F - e * G) * invEGF2 * dpDu + (e * F - f * E) * invEGF2 * dpDv;
			Vector3D dnDv = (g * F - f * G) * invEGF2 * dpDu + (f * F - g * E) * invEGF2 * dpDv;

			// Initialize differenterial geometry from parametric information.
			dg = new DifferentialGeometry(ObjectToWorld.Transform(pHit), ObjectToWorld.Transform(dpDu),
				ObjectToWorld.Transform(dpDv), ObjectToWorld.Transform(dnDu), ObjectToWorld.Transform(dnDv),
				u, v, this);

			tHit = tHitTemp;
			return true;
		}
	}
}