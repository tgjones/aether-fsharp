using System.Windows.Media.Imaging;
using Caliburn.Micro;

namespace Aether.Studio.Framework
{
	public abstract class RenderedDocumentBase : Screen
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