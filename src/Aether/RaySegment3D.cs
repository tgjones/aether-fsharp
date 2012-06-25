using System;
using Nexus;

namespace Aether
{
    public class RaySegment3D
    {
        public const float Epsilon = 1e-3f;

        public Point3D Origin { get; private set; }
        public Vector3D Direction { get; private set; }
        public float MinT { get; set; }
        public float MaxT { get; set; }
        public float Time { get; private set; }

        public RaySegment3D(Point3D origin, Vector3D direction,
                            float minT = Epsilon, float maxT = float.MaxValue,
                            float time = 0)
        {
            Origin = origin;
            Direction = direction;
            MinT = minT;
            MaxT = maxT;
            Time = time;
        }

        public Point3D Evaluate(float t)
        {
            return Origin + Direction * t;
        }
    }
}

