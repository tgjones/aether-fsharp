using Aether.Sampling;
using Nexus;

namespace Aether.Films
{
    public abstract class Film
    {
        private readonly int _xRes;
        private readonly int _yRes;

    	public int XRes
		{
			get { return _xRes; }
		}

		public int YRes
		{
			get { return _yRes; }
		}

    	public float AspectRatio
    	{
    		get { return XRes / (float) YRes; }
    	}

    	protected Film(int xRes, int yRes)
        {
            _xRes = xRes;
            _yRes = yRes;
        }

    	public abstract void AddSample(Sample sample, ColorF c);
    }
}