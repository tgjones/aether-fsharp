using Nexus;

namespace Aether.Sampling
{
	public class RegularSampler : Sampler
	{
		private readonly int _xStart, _yStart;
		private readonly int _xEnd, _yEnd;
		private int _xPos, _yPos;

		public RegularSampler(IntPoint2D start, IntPoint2D end)
			: base(start, end)
		{
			_xStart = _xPos = start.X;
			_yStart = _yPos = start.Y;
			_xEnd = end.X;
			_yEnd = end.Y;
		}

		public override bool GetNextSample(out Sample sample)
		{
			if (_yPos == _yEnd)
			{
				sample = null;
				return false;
			}

			sample = new Sample { ImageX = _xPos, ImageY = _yPos };

			if (++_xPos == _xEnd)
			{
				_xPos = _xStart;
				++_yPos;
			}

			return true;
		}
	}
}