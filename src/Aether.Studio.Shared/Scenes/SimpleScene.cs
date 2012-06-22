using Aether.Films;
using Aether.Cameras;
using Aether.Sampling;
using Aether.Shapes;

namespace Aether.Studio.Shared
{
    public class SimpleScene : DemoSceneBase
    {
        public override Scene CreateScene(Film film)
        {
            var camera = new PerspectiveCamera(film,
                                                1.0f, 100.0f, Nexus.Vector3D.Forward, Nexus.Vector3D.Up,
                                                new Nexus.Point3D(0, 0, 15), Nexus.MathUtility.PI_OVER_4);

            var sampler = new RegularSampler(new Nexus.IntPoint2D(0, 0), new Nexus.IntPoint2D(film.XRes, film.YRes));

            var shape = new Sphere(new Nexus.Point3D(0, 0, 0), 5);

            return new Scene(camera, sampler, shape);
        }
    }
}

