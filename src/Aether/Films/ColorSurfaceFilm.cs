using Aether.Sampling;
using Nexus;
using Nexus.Graphics;

namespace Aether.Films
{
	public class ColorSurfaceFilm : Film
	{
		private readonly ColorSurface _surface;

		public ColorSurfaceFilm(int xRes, int yRes, int multiSampleCount)
			: base(xRes, yRes)
		{
			_surface = new ColorSurface(xRes, yRes, multiSampleCount);
		}

		public override void AddSample(Sample sample, ColorF c)
		{
			_surface[sample.ImageX, sample.ImageY, 0] = c;
		}

		public void Present(IImageBuffer imageBuffer)
		{
			_surface.Resolve(imageBuffer);
		}
	}
}