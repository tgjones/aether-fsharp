using System.Collections.Generic;
using Nexus;

namespace Aether.Materials.Reflectance
{
	public class Bsdf
	{
		private readonly DifferentialGeometry _dg;
		private readonly Normal3D _normal;
		private readonly List<Bxdf> _bxdfs;

		public Bsdf(DifferentialGeometry dg, Normal3D normal)
		{
			_dg = dg;
			_normal = normal;
			_bxdfs = new List<Bxdf>();
		}

		public void Add(Bxdf bxdf)
		{
			_bxdfs.Add(bxdf);
		}
	}
}