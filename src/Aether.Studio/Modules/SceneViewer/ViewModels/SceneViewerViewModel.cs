using Aether.Studio.Framework;
using Aether.Studio.Modules.SceneViewer.Scenes;

namespace Aether.Studio.Modules.SceneViewer.ViewModels
{
	public class SceneViewerViewModel : RenderedDocumentBase
	{
		private readonly SceneFactoryBase _sceneFactory;

		public SceneViewerViewModel(SceneFactoryBase sceneFactory)
		{
		    DisplayName = sceneFactory.GetType().Name;
			_sceneFactory = sceneFactory;
		}

		protected override void Draw()
		{
			var scene = _sceneFactory.CreateScene(OutputBitmap);
			scene.Render();
		}
	}
}