using System.Windows.Media.Imaging;

namespace Aether.Studio.Modules.SceneViewer.Scenes
{
    public class FileSceneFactory : SceneFactoryBase
    {
        private readonly string _pbrtFileName;

        public FileSceneFactory(string pbrtFileName)
        {
            _pbrtFileName = pbrtFileName;
        }

        public override Scene CreateScene(WriteableBitmap bitmap)
        {
            throw new System.NotImplementedException();
        }
    }
}