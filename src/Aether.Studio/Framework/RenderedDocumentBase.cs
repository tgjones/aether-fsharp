using System.Windows.Media.Imaging;
using Gemini.Framework;

namespace Aether.Studio.Framework
{
	public abstract class RenderedDocumentBase : Document
	{
		private WriteableBitmap _outputBitmap;
		public WriteableBitmap OutputBitmap
		{
			get { return _outputBitmap; }
			set
			{
				_outputBitmap = value;
				if (value != null)
					Draw();
			}
		}

		protected abstract void Draw();
	}
}