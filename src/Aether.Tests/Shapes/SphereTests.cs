using Aether.Shapes;
using NUnit.Framework;
using Nexus;
using Nexus.Graphics.Transforms;
using Sphere = Aether.Shapes.Sphere;

namespace Aether.Tests.Shapes
{
	[TestFixture]
	public class SphereTests
	{
		[Test]
		public void CanCreateInstance()
		{
			var sphere = new Sphere(new TranslateTransform { OffsetX = 1, OffsetY = 2, OffsetZ = 3 }, 10.0f);
			Assert.That(sphere.Radius, Is.EqualTo(10.0f));
		}

		[Test]
		public void HitReturnsTrueWhenRayHits()
		{
			var sphere = new Sphere(new TranslateTransform { OffsetZ = 3 }, 10.0f);
			var ray = new Ray3D(new Point3D(0, 0, 20), Vector3D.Forward);
			float tHit;
			DifferentialGeometry dg;
			Assert.That(sphere.TryIntersect(ray, float.MinValue, float.MaxValue, out tHit, out dg), Is.True);
			Assert.That(tHit, Is.EqualTo(7.0f));
		}

		[Test]
		public void HitReturnsFalseWhenRayMisses()
		{
			var sphere = new Sphere(new TranslateTransform { OffsetZ = 3 }, 10.0f);
			var ray = new Ray3D(new Point3D(20, 0, 20), Vector3D.Forward);
			Assert.That(sphere.Intersects(ray, float.MinValue, float.MaxValue), Is.False);
		}
	}
}