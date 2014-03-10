using System.ComponentModel.Composition;
using Gemini.Framework;

namespace Aether.Studio.Modules.Startup
{
	[Export(typeof(IModule))]
	public class Module : ModuleBase
	{
		public override void Initialize()
		{
            MainWindow.Title = "Aether Studio";
		}
	}
}