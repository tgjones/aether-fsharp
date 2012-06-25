using Nexus;
using Nexus.Graphics.Transforms;

namespace Aether.Util
{
    public static class ExtensionMethods
    {
        public static RaySegment3D Transform(this Transform3D transform, RaySegment3D ray)
        {
            var transformedRay = transform.Transform(new Ray3D(ray.Origin, ray.Direction));
            return new RaySegment3D(transformedRay.Origin, transformedRay.Direction,
                                    ray.MinT, ray.MaxT, ray.Time);
        }

        public static bool IsEmpty(this ColorF color)
        {
            // TODO: Should use == operator
            ColorF black = ColorsF.Black;
            return color.A == black.A && color.R == black.R
                && color.G == black.G && color.B == black.B;
        }
    }
}

