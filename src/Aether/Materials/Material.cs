using Aether.Materials.Reflectance;

namespace Aether.Materials
{
	public abstract class Material
	{
		public abstract Bsdf GetBsdf(DifferentialGeometry dg);
	}
}