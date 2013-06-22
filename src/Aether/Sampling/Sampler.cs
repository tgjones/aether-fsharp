using System.Collections.Generic;
using Nexus;

namespace Aether.Sampling
{
	public abstract class Sampler
	{
		private readonly IntPoint2D _start;
		private readonly IntPoint2D _end;

		protected Sampler(IntPoint2D start, IntPoint2D end)
		{
			_start = start;
			_end = end;
		}

		public abstract IEnumerable<Sample> GetSamples();
	}
}