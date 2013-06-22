using System.Linq;
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
			var sampler = new RegularSampler(new IntPoint2D(0, 0), new IntPoint2D(2, 2));

		    var samples = sampler.GetSamples().ToList();

		    Assert.That(samples, Has.Count.EqualTo(4));

			Assert.That(samples[0].ImageX, Is.EqualTo(0));
            Assert.That(samples[0].ImageY, Is.EqualTo(0));

            Assert.That(samples[1].ImageX, Is.EqualTo(1));
            Assert.That(samples[1].ImageY, Is.EqualTo(0));

            Assert.That(samples[2].ImageX, Is.EqualTo(0));
            Assert.That(samples[2].ImageY, Is.EqualTo(1));

            Assert.That(samples[3].ImageX, Is.EqualTo(1));
            Assert.That(samples[3].ImageY, Is.EqualTo(1));
		}
	}
}