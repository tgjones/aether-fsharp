using System.Windows.Media.Imaging;

namespace Aether.Studio.Modules.DemoSceneViewer.Scenes
{
	public abstract class DemoSceneBase
	{
		public abstract Scene CreateScene(WriteableBitmap bitmap);
	}
}