using System.Collections.Generic;
using Aether.Cameras;
using Aether.Films;
using Aether.Integrators;
using Aether.Lights;
using Aether.Materials;
using Aether.Primitives;
using Aether.Sampling;
using Nexus;
using Nexus.Graphics.Colors;
using Nexus.Graphics.Transforms;
using Sphere = Aether.Shapes.Sphere;

namespace Aether.Studio.Modules.DemoSceneViewer.Scenes
{
	public class SimpleTriangleScene : DemoSceneBase
	{
		public override Scene CreateScene(Film film)
		{
			var camera = new OrthographicCamera(film,
				1.0f, 100.0f, Vector3D.Forward, Vector3D.Up,
				new Point3D(0, 0, 10), 20);
			//var camera = new PerspectiveCamera(film,
			//    1.0f, 100.0f, Nexus.Vector3D.Forward, Nexus.Vector3D.Up,
			//    new Nexus.Point3D(0, 0, 10),
			//    MathUtility.PI_OVER_4);

			var surfaceIntegrator = new WhittedIntegrator(6);

			var sampler = new RegularSampler(
				new IntPoint2D(0, 0),
				new IntPoint2D(film.XRes, film.YRes));

			var shape = new Sphere(new TranslateTransform(), 5);
			var primitive = new GeometricPrimitive(shape, new MatteMaterial(ColorsF.Green));

			var lights = new List<Light>();
			lights.Add(new DirectionalLight(new TranslateTransform(), Vector3D.Down, ColorsF.White));

			return new Scene(camera, surfaceIntegrator, sampler, primitive, lights);
		}
	}
}