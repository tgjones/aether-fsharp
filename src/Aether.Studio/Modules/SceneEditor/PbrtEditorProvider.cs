using System.ComponentModel.Composition;
using System.IO;
using Aether.Studio.Modules.SceneEditor.ViewModels;
using Caliburn.Micro;
using Gemini.Framework;
using Gemini.Framework.Services;

namespace Aether.Studio.Modules.SceneEditor
{
    [Export(typeof(IEditorProvider))]
    public class PbrtEditorProvider : IEditorProvider
    {
        public bool Handles(string path)
        {
            return Path.GetExtension(path).ToLower() == ".pbrt";
        }

        public IDocument Create(string path)
        {
            var sceneEditorViewModel = IoC.Get<SceneEditorViewModel>();
            sceneEditorViewModel.Open(path);
            return sceneEditorViewModel;
        }
    }
}