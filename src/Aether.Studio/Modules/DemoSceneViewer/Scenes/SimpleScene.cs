using System.Collections.Generic;
using Aether.Cameras;
using Aether.Films;
using Aether.Geometry;
using Aether.Integrators;
using Aether.Lights;
using Aether.Materials;
using Aether.Math;
using Aether.Primitives;
using Aether.Sampling;
using Aether.Shapes;
using Aether.Transforms;

namespace Aether.Studio.Modules.DemoSceneViewer.Scenes
{
	public class SimpleTriangleScene : DemoSceneBase
	{
		public override Scene CreateScene(Film film)
		{
			var camera = new OrthographicCamera(
                Transform.LookAt(new Point(0, 0, 10), new Point(0, 0, 0), new Vector(0, 1, 0)),
                new float[] { -2, 2, -1, 1 }, 0.0f, 1.0f, 0.0f, 1e30f, film);

			var surfaceIntegrator = new WhittedIntegrator(6);

			var sampler = new RegularSampler(
				0, film.XRes,
                0, film.YRes);

            var shape = new Sphere(Transform.Translate(Vector.Zero), false, 5, -4.9f, 4.9f, 360.0f);
            var primitive = new GeometricPrimitive(shape, new MatteMaterial(new RgbSpectrum(new [] { 0.0f, 1.0f, 0.0f}))); // Green

            var lights = new List<Light>();
            lights.Add(new DistantLight(Transform.Translate(Vector.Zero),
                new RgbSpectrum(new[] { 1.0f, 1.0f, 1.0f }), new Vector(0, -1, 0))); // White.

            return new Scene(camera, surfaceIntegrator, sampler, primitive, lights);
		}
	}
}