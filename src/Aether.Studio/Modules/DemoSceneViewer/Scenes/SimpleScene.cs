using System;
using System.Collections.Generic;
using Aether.Cameras;
using Aether.Films;
using Aether.Geometry;
using Aether.Integrators;
using Aether.Lights;
using Aether.Materials;
using Aether.Primitives;
using Aether.Sampling;
using Aether.Shapes;
using Aether.Transforms;
using Nexus;
using Nexus.Graphics.Colors;

namespace Aether.Studio.Modules.DemoSceneViewer.Scenes
{
	public class SimpleTriangleScene : DemoSceneBase
	{
		public override Scene CreateScene(Film film)
		{
			var camera = new OrthographicCamera(
                TransformModule.LookAt(new Point(0, 0, -3), new Point(.2f, -.2f, 0), new Vector(0, 1, 0)),
                new float[] { -2, 2, -1, 1 }, 0.0f, 1.0f, 0.0f, 1e30f, film);

			var surfaceIntegrator = new WhittedIntegrator(6);

			var sampler = new RegularSampler(
				new IntPoint2D(0, 0),
				new IntPoint2D(film.XRes, film.YRes));

            var shape = new Sphere(TransformModule.Translate(VectorModule.Zero), false, 5);
            var primitive = new GeometricPrimitive(shape, new MatteMaterial(new Spectrum())); // Green

            var lights = new List<Light>();
            lights.Add(new DistantLight(TransformModule.Translate(VectorModule.Zero), 
                new Spectrum(), new Vector(0, -1, 0))); // White.

            return new Scene(camera, surfaceIntegrator, sampler, primitive, lights);
		}
	}
}