using Aether.Shapes;
using NUnit.Framework;
using Nexus;
using Nexus.Graphics.Transforms;
using Plane = Aether.Shapes.Plane;

namespace Aether.Tests.Shapes
{
	[TestFixture]
	public class PlaneTests
	{
		[Test]
		public void CanCreateInstance()
		{
			var point = new Point3D(1, 2, 3);
			var normal = new Normal3D(0, 1, 0);
			var plane = new Plane(new TranslateTransform { OffsetX = 1, OffsetY = 2, OffsetZ = 3 }, point, normal);
			Assert.That(plane.Point, Is.EqualTo(point));
			Assert.That(plane.Normal, Is.EqualTo(normal));
		}

		[Test]
		public void HitReturnsTrueWhenRayHits()
		{
			var plane = new Plane(new TranslateTransform(), new Point3D(0, 0, 0), new Normal3D(0, 1, 0));
			var ray = new Ray3D(new Point3D(0, 4, 0), Vector3D.Down);
			float tHit;
			DifferentialGeometry dg;
			Assert.That(plane.TryIntersect(ray, float.MinValue, float.MaxValue, out tHit, out dg), Is.True);
			Assert.That(tHit, Is.EqualTo(4.0f));
		}

		[Test]
		public void HitReturnsFalseWhenRayMisses()
		{
			var plane = new Plane(new TranslateTransform(), new Point3D(0, 0, 0), new Normal3D(0, 1, 0));
			var ray = new Ray3D(new Point3D(0, 1, 0), Vector3D.Forward);
			float tHit;
			DifferentialGeometry dg;
			Assert.That(plane.TryIntersect(ray, float.MinValue, float.MaxValue, out tHit, out dg), Is.False);
		}
	}
}