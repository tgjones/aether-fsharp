using System.Windows.Controls;
using Gemini.Modules.CodeEditor.Views;

namespace Aether.Studio.Modules.SceneEditor.Views
{
    public partial class SceneEditorView : UserControl, ICodeEditorView
    {
        public ICSharpCode.AvalonEdit.TextEditor TextEditor
        {
            get { return CodeEditor; }
        }

        public SceneEditorView()
        {
            InitializeComponent();
        }
    }
}
