using System.Windows.Media.Imaging;
using Aether.Sampling;
using Nexus;
using Nexus.Graphics;
using Nexus.Util;

namespace Aether.Films
{
	public class ColorSurfaceFilm : Film
	{
		private readonly WriteableBitmapWrapper _writeableBitmap;
		private readonly ColorSurface _surface;

		public ColorSurfaceFilm(WriteableBitmap writeableBitmap, int xRes, int yRes, int multiSampleCount)
			: base(xRes, yRes)
		{
			_writeableBitmap = new WriteableBitmapWrapper(writeableBitmap);
			_surface = new ColorSurface(xRes, yRes, multiSampleCount);
		}

		public override void AddSample(Sample sample, ColorF c)
		{
			_surface[sample.ImageX, sample.ImageY, 0] = c;
		}

		public void Present()
		{
			_surface.Resolve(new WriteableBitmapBuffer(_writeableBitmap));
			_writeableBitmap.Invalidate();
		}
	}
}