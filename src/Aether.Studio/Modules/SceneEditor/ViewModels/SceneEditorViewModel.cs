using System;
using System.ComponentModel.Composition;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Media.Imaging;
using Gemini.Modules.CodeEditor;
using Gemini.Modules.CodeEditor.ViewModels;
using Gemini.Modules.CodeEditor.Views;

namespace Aether.Studio.Modules.SceneEditor.ViewModels
{
    [Export(typeof(SceneEditorViewModel))]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class SceneEditorViewModel : CodeEditorViewModel
    {
        private WriteableBitmap _outputBitmap;
        private ICodeEditorView _codeEditorView;
        private CancellationTokenSource _cancellationTokenSource;

        public WriteableBitmap OutputBitmap
        {
            get { return _outputBitmap; }
            set
            {
                _outputBitmap = value;
                NotifyOfPropertyChange(() => OutputBitmap);
            }
        }

        [ImportingConstructor]
        public SceneEditorViewModel(LanguageDefinitionManager languageDefinitionManager)
            : base(languageDefinitionManager)
        {
        }

        protected override void OnViewLoaded(object view)
        {
            base.OnViewLoaded(view);

            _codeEditorView = (ICodeEditorView) view;
            _codeEditorView.TextEditor.TextChanged += OnTextChanged;

            StartRendering(_codeEditorView.TextEditor.Text);
        }

        private void OnTextChanged(object sender, EventArgs e)
        {
            // Cancel any existing render task.
            if (_cancellationTokenSource != null)
                _cancellationTokenSource.Cancel();

            StartRendering(_codeEditorView.TextEditor.Text);
        }

        private void StartRendering(string pbrtCode)
        {
            _cancellationTokenSource = new CancellationTokenSource();
            var token = _cancellationTokenSource.Token;
            var scene = SceneReader.Read(new StringReader(pbrtCode));
            OutputBitmap = scene.Output;
            Task.Factory.StartNew(() => scene.Render(token), token);
        }
    }
}