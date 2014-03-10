using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using Aether.Studio.Modules.SceneViewer.Scenes;
using Aether.Studio.Modules.SceneViewer.ViewModels;
using Caliburn.Micro;
using Gemini.Framework;
using Gemini.Framework.Results;
using Gemini.Modules.CodeEditor.ViewModels;
using Gemini.Modules.MainMenu.Models;

namespace Aether.Studio.Modules.SceneViewer
{
	[Export(typeof(IModule))]
	public class Module : ModuleBase
	{
		public override void Initialize()
		{
			var fileMenuItem = Shell.MainMenu.First(mi => mi.Name == "File");
			var demoSceneMenuItem = new MenuItem("Demo Scenes");
			fileMenuItem.Children.Insert(0, demoSceneMenuItem);
			fileMenuItem.Children.Insert(1, new MenuItemSeparator());

			demoSceneMenuItem.Add(
				new MenuItem("First", OpenDemoScene<SimpleTriangleSceneFactory>));

		    var renderMenuItem = new MenuItem("_Render");
            Shell.MainMenu.Add(renderMenuItem);

            renderMenuItem.Children.Add(new MenuItem("Render Now", RenderNow, 
                () => Shell.ActiveItem is CodeEditorViewModel));
		}

        private static IEnumerable<IResult> OpenDemoScene<TSceneFactory>()
			where TSceneFactory : SceneFactoryBase, new()
		{
            yield return Show.Document(new SceneViewerViewModel(new TSceneFactory()));
		}

        private IEnumerable<IResult> RenderNow()
        {
            var codeEditor = Shell.ActiveItem as CodeEditorViewModel;
            if (codeEditor == null)
                yield break;

            yield return Show.Document(new SceneViewerViewModel(
                new FileSceneFactory(codeEditor.Path)));
        }
	}
}