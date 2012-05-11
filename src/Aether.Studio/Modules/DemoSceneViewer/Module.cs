using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using Aether.Studio.Modules.DemoSceneViewer.Scenes;
using Aether.Studio.Modules.DemoSceneViewer.ViewModels;
using Caliburn.Micro;
using Gemini.Framework;
using Gemini.Framework.Menus;
using Gemini.Framework.Results;

namespace Aether.Studio.Modules.DemoSceneViewer
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
				new MenuItem("First", OpenDemoScene<SimpleTriangleScene>));
		}

		private static IEnumerable<IResult> OpenDemoScene<TScene>()
			where TScene : DemoSceneBase, new()
		{
			yield return Show.Document(new DemoSceneViewerViewModel(new TScene()));
		}
	}
}