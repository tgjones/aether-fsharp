using System.Collections.Generic;
using System.Windows.Media.Imaging;
using Aether.Cameras;
using Aether.Films;
using Aether.Filters;
using Aether.Geometry;
using Aether.Integrators;
using Aether.Lights;
using Aether.Materials;
using Aether.Primitives;
using Aether.Sampling;
using Aether.Shapes;
using Aether.Textures;
using Aether.Transforms;

namespace Aether.Studio.Modules.DemoSceneViewer.Scenes
{
	public class SimpleTriangleScene : DemoSceneBase
	{
		public override Scene CreateScene(WriteableBitmap bitmap)
		{
            var film = new ImageFilm(bitmap, new BoxFilter(0.5f, 0.5f), new CropWindow(0, 1, 0, 1));
			var camera = new OrthographicCamera(
                Transform.LookAt(new Point(0, 0, 10), new Point(0, 0, 0), new Vector(0, 1, 0)),
                new CropWindow(-1, 1, -1, 1), // TODO: This needs to respect aspect ratio.
                0.0f, 1.0f, 0.0f, 1e30f, film);

			var surfaceIntegrator = new WhittedIntegrator(6);

			var sampler = new StratifiedSampler(
				0, film.XRes,
                0, film.YRes,
                1, 1,
                false,
                0.0f, 1.0f);

            var shape = new Sphere(Transform.Translate(Vector.Zero), false, 5, -4.9f, 4.9f, 360.0f);
		    var material = new MatteMaterial(
                new ConstantTexture<Spectrum>(new Spectrum(0.0f, 1.0f, 0.0f)), // Green
                new ConstantTexture<float>(0.0f)); 
            var primitive = new GeometricPrimitive(shape, material);

            var lights = new List<Light>();
            lights.Add(new DistantLight(Transform.Translate(Vector.Zero),
                new Spectrum(1.0f, 1.0f, 1.0f), new Vector(0, -1, 0))); // White.

            return new Scene(camera, surfaceIntegrator, sampler, primitive, lights);
		}
	}
}