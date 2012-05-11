using Nexus;

namespace Aether.Materials.Reflectance
{
	public class Lambertian : Bxdf
	{
		private ColorF _reflectance;

		public Lambertian(ColorF reflectance)
			: base(BxdfType.Reflection | BxdfType.Diffuse)
		{
			_reflectance = reflectance;
		}
	}
}