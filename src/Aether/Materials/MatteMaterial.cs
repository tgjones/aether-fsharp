using Aether.Materials.Reflectance;
using Aether.Shapes;
using Nexus.Graphics.Colors;

namespace Aether.Materials
{
	public class MatteMaterial : Material
	{
		private readonly ColorF _kd;

		public MatteMaterial(ColorF kd)
		{
			_kd = kd;
		}

		public override Bsdf GetBsdf(DifferentialGeometry dg)
		{
			var bsdf = new Bsdf(dg, dg.Normal);
			bsdf.Add(new Lambertian(_kd));
			return bsdf;
		}
	}
}