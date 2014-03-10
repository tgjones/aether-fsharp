using System.Windows.Media.Imaging;

namespace Aether.Studio.Modules.SceneViewer.Scenes
{
	public abstract class SceneFactoryBase
	{
		public abstract Scene CreateScene(WriteableBitmap bitmap);
	}
}