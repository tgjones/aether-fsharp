using NUnit.Framework;
using Nexus;
using Sphere = Aether.Shapes.Sphere;

namespace Aether.Tests.Shapes
{
	[TestFixture]
	public class SphereTests
	{
		[Test]
		public void CanCreateInstance()
		{
			var point = new Point3D(1, 2, 3);
			var sphere = new Sphere(point, 10.0f);
			Assert.That(sphere.Center, Is.EqualTo(point));
			Assert.That(sphere.Radius, Is.EqualTo(10.0f));
		}

		[Test]
		public void HitReturnsTrueWhenRayHits()
		{
			var sphere = new Sphere(new Point3D(0, 0, 3), 10.0f);
			var ray = new Ray3D(new Point3D(0, 0, 20), Vector3D.Forward);
			float tMin;
			ShadeRec sr;
			Assert.That(sphere.Hit(ray, out tMin, out sr), Is.True);
			Assert.That(7.0f, Is.EqualTo(tMin));
		}

		[Test]
		public void HitReturnsFalseWhenRayMisses()
		{
			var sphere = new Sphere(new Point3D(0, 0, 3), 10.0f);
			var ray = new Ray3D(new Point3D(20, 0, 20), Vector3D.Forward); float tMin;
			ShadeRec sr;
			Assert.That(sphere.Hit(ray, out tMin, out sr), Is.False);
		}
	}
}