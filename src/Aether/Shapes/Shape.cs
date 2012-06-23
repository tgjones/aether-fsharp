using Nexus;
using Nexus.Graphics.Transforms;

namespace Aether.Shapes
{
	public abstract class Shape
	{
		private readonly Transform3D _objectToWorld;

		public abstract AxisAlignedBoundingBox ObjectSpaceBounds { get; }

		public virtual AxisAlignedBoundingBox WorldSpaceBounds
		{
			get { return _objectToWorld.Transform(ObjectSpaceBounds); }
		}

		protected Shape(Transform3D objectToWorld)
		{
			_objectToWorld = objectToWorld;
		}

		public abstract bool TryIntersect(Ray3D ray, out float tHit, out DifferentialGeometry dg);

		public virtual bool Intersects(Ray3D ray)
		{
			float tHit;
			DifferentialGeometry dg;
			return TryIntersect(ray, out tHit, out dg);
		}
	}
}