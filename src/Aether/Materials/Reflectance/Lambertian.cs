using Nexus;
using Nexus.Graphics.Colors;

namespace Aether.Materials.Reflectance
{
	public class Lambertian : Bxdf
	{
		private readonly ColorF _reflectance;

		public Lambertian(ColorF reflectance)
			: base(BxdfType.Reflection | BxdfType.Diffuse)
		{
			_reflectance = reflectance;
		}

		public override ColorF Evaluate(Vector3D incoming, Vector3D outgoing)
		{
			return _reflectance;
		}
	}
}