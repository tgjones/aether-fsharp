using Aether.Materials.Reflectance;
using Aether.Shapes;

namespace Aether.Materials
{
	public abstract class Material
	{
		public abstract Bsdf GetBsdf(DifferentialGeometry dg);
	}
}