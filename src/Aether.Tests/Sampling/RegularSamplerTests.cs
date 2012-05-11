using Aether.Sampling;
using NUnit.Framework;
using Nexus;

namespace Aether.Tests.Sampling
{
	[TestFixture]
	public class RegularSamplerTests
	{
		[Test]
		public void GetNextSampleReturnsCorrectSamples()
		{
			var s = new RegularSampler(new IntPoint2D(0, 0), new IntPoint2D(2, 2));

			Sample sample;

			Assert.That(s.GetNextSample(out sample), Is.True);
			Assert.That(sample.ImageX, Is.EqualTo(0));
			Assert.That(sample.ImageY, Is.EqualTo(0));

			Assert.That(s.GetNextSample(out sample), Is.True);
			Assert.That(sample.ImageX, Is.EqualTo(1));
			Assert.That(sample.ImageY, Is.EqualTo(0));

			Assert.That(s.GetNextSample(out sample), Is.True);
			Assert.That(sample.ImageX, Is.EqualTo(0));
			Assert.That(sample.ImageY, Is.EqualTo(1));

			Assert.That(s.GetNextSample(out sample), Is.True);
			Assert.That(sample.ImageX, Is.EqualTo(1));
			Assert.That(sample.ImageY, Is.EqualTo(1));

			Assert.That(s.GetNextSample(out sample), Is.False);
		}
	}
}