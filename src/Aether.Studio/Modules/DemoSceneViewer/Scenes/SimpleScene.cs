using Aether.Cameras;
using Aether.Films;
using Aether.Sampling;
using Nexus;
using Sphere = Aether.Shapes.Sphere;

namespace Aether.Studio.Modules.DemoSceneViewer.Scenes
{
	public class SimpleTriangleScene : DemoSceneBase
	{
		public override Scene CreateScene(Film film)
		{
			var camera = new OrthographicCamera(film,
				1.0f, 100.0f, Nexus.Vector3D.Forward, Nexus.Vector3D.Up,
				new Nexus.Point3D(0, 0, 10), 20);
			//var camera = new PerspectiveCamera(film,
			//    1.0f, 100.0f, Nexus.Vector3D.Forward, Nexus.Vector3D.Up,
			//    new Nexus.Point3D(0, 0, 10),
			//    MathUtility.PI_OVER_4);

			var sampler = new RegularSampler(
				new Nexus.IntPoint2D(0, 0),
				new Nexus.IntPoint2D(film.XRes, film.YRes));

			var shape = new Sphere(new Nexus.Point3D(0, 0, 0), 5);

			return new Scene(camera, sampler, shape);
		}
	}
}