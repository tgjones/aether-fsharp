using System.Collections.Generic;
using Nexus;

namespace Aether.Sampling
{
	public class RegularSampler : Sampler
	{
		private readonly int _xStart, _yStart;
		private readonly int _xEnd, _yEnd;

		public RegularSampler(IntPoint2D start, IntPoint2D end)
			: base(start, end)
		{
			_xStart = start.X;
			_yStart = start.Y;
			_xEnd = end.X;
			_yEnd = end.Y;
		}

	    public override IEnumerable<Sample> GetSamples()
	    {
            var xPos = _xStart;
	        var yPos = _yStart;

	        while (yPos < _yEnd)
	        {
	            yield return new Sample { ImageX = xPos, ImageY = yPos };

	            if (++xPos == _xEnd)
	            {
	                xPos = _xStart;
	                ++yPos;
	            }
	        }
	    }
	}
}