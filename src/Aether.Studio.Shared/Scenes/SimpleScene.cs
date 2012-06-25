using Aether.Films;
using Aether.Cameras;
using Aether.Sampling;
using Aether.Shapes;
using Nexus.Graphics.Transforms;
using Aether.Integrators;
using Nexus;
using Aether.Primitives;
using Aether.Lights;
using System.Collections.Generic;
using Aether.Materials;

namespace Aether.Studio.Shared
{
    public class SimpleScene : DemoSceneBase
    {
        public override Scene CreateScene(Film film)
        {
            var camera = new PerspectiveCamera(film,
                1.0f, 100.0f, Vector3D.Forward, Vector3D.Up,
                new Point3D(0, 0, 15), MathUtility.PI_OVER_4);

            var surfaceIntegrator = new WhittedIntegrator(6);

            var sampler = new RegularSampler(
                new IntPoint2D(0, 0),
                new IntPoint2D(film.XRes, film.YRes));

            var shape = new Aether.Shapes.Sphere(new TranslateTransform(), 5);
            var material = new MatteMaterial(ColorsF.Red);
            var primitive = new GeometricPrimitive(shape, material);

            var lights = new List<Light>
            {
//                new PointLight(
//                    new TranslateTransform { OffsetX = 10, OffsetY = 10, OffsetZ = 10},
//                    new ColorF(0.5f))
                new DirectionalLight(new RotateTransform3D(), Vector3D.Down, new ColorF(0.5f))
            };

            return new Scene(camera, surfaceIntegrator, sampler, primitive,
                             lights);
        }
    }
}

