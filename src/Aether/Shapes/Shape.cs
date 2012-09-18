﻿using Nexus.Graphics.Transforms;
using Nexus.Objects3D;

namespace Aether.Shapes
{
	public abstract class Shape
	{
		private readonly Transform3D _objectToWorld;
		private readonly Transform3D _worldToObject;

		public abstract AxisAlignedBox3D ObjectSpaceBounds { get; }

		public virtual AxisAlignedBox3D WorldSpaceBounds
		{
			get { return _objectToWorld.Transform(ObjectSpaceBounds); }
		}

		public Transform3D ObjectToWorld
		{
			get { return _objectToWorld; }
		}

		public Transform3D WorldToObject
		{
			get { return _worldToObject; }
		}

		protected Shape(Transform3D objectToWorld)
		{
			_objectToWorld = objectToWorld;
			_worldToObject = objectToWorld.Inverse;
		}

		public abstract bool TryIntersect(RaySegment3D ray, out float tHit, out DifferentialGeometry dg);

		public virtual bool Intersects(RaySegment3D ray)
		{
			float tHit;
			DifferentialGeometry dg;
			return TryIntersect(ray, out tHit, out dg);
		}
	}
}