using NUnit.Framework;
using Nexus;
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
			var plane = new Plane(point, normal);
			Assert.That(plane.Point, Is.EqualTo(point));
			Assert.That(plane.Normal, Is.EqualTo(normal));
		}

		[Test]
		public void HitReturnsTrueWhenRayHits()
		{
			var plane = new Plane(new Point3D(0, 0, 0), new Normal3D(0, 1, 0));
			var ray = new Ray3D(new Point3D(0, 4, 0), Vector3D.Down);
			float tMin;
			ShadeRec sr;
			Assert.That(plane.Hit(ray, out tMin, out sr), Is.True);
			Assert.That(tMin, Is.EqualTo(4.0f));
		}

		[Test]
		public void HitReturnsFalseWhenRayMisses()
		{
			var plane = new Plane(new Point3D(0, 0, 0), new Normal3D(0, 1, 0));
			var ray = new Ray3D(new Point3D(0, 1, 0), Vector3D.Forward);
			float tMin;
			ShadeRec sr;
			Assert.That(plane.Hit(ray, out tMin, out sr), Is.False);
		}
	}
}