using Nexus;

namespace Aether.Shapes
{
	public abstract class Shape
	{
		public abstract bool Hit(Ray3D ray, out float tMin, out ShadeRec shadeRec);
	}
}