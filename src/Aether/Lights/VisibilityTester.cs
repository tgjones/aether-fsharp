using Nexus;

namespace Aether.Lights
{
    public class VisibilityTester
    {
        private readonly RaySegment3D _ray;

        public VisibilityTester(Point3D point, Vector3D w)
        {
            _ray = new RaySegment3D(point, w, RaySegment3D.Epsilon);
        }

        public VisibilityTester(Point3D point1, Point3D point2)
        {
            _ray = new RaySegment3D(point1, point2 - point1,
                                    RaySegment3D.Epsilon,
                                    1.0f - RaySegment3D.Epsilon);
        }

        public bool Unoccluded(Scene scene)
        {
            return !scene.Intersects(_ray);
        }
    }
}

