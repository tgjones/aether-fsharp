using Nexus;
using Nexus.Graphics.Transforms;

namespace Aether.Shapes
{
	public abstract class Shape
	{
		private readonly Transform3D _objectToWorld;
		private readonly Transform3D _worldToObject;

		public abstract AxisAlignedBoundingBox ObjectSpaceBounds { get; }

		public virtual AxisAlignedBoundingBox WorldSpaceBounds
		{
			get { return _objectToWorld.Transform(ObjectSpaceBounds); }
		}

		protected Transform3D ObjectToWorld
		{
			get { return _objectToWorld; }
		}

		protected Transform3D WorldToObject
		{
			get { return _worldToObject; }
		}

		protected Shape(Transform3D objectToWorld)
		{
			_objectToWorld = objectToWorld;
			_worldToObject = objectToWorld.Inverse;
		}

		public abstract bool TryIntersect(Ray3D ray, float tMin, float tMax, out float tHit, out DifferentialGeometry dg);

		public virtual bool Intersects(Ray3D ray, float tMin, float tMax)
		{
			float tHit;
			DifferentialGeometry dg;
			return TryIntersect(ray, tMin, tMax, out tHit, out dg);
		}
	}
}