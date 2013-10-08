using System;
using Aether.Films;
using Aether.Studio.Framework;
using Aether.Studio.Modules.DemoSceneViewer.Scenes;

namespace Aether.Studio.Modules.DemoSceneViewer.ViewModels
{
	public class DemoSceneViewerViewModel : RenderedDocumentBase
	{
		private readonly Type _sceneType;
		private readonly DemoSceneBase _scene;

		public override string DisplayName
		{
			get { return _sceneType.Name; }
		}

		public DemoSceneViewerViewModel(DemoSceneBase scene)
		{
			_sceneType = scene.GetType();
			_scene = scene;
		}

		protected override void Draw()
		{
			var scene = _scene.CreateScene(OutputBitmap);
			scene.Render();
		}
	}
}