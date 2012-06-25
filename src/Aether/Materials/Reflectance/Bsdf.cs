using System.Collections.Generic;
using Aether.Shapes;
using Nexus;

namespace Aether.Materials.Reflectance
{
	public class Bsdf
	{
		private readonly DifferentialGeometry _dg;
		private readonly Normal3D _geometricNormal;
		private readonly List<Bxdf> _bxdfs;

		private readonly Normal3D _nn;
		private readonly Vector3D _sn, _tn;

        public DifferentialGeometry DifferentialGeometry
        {
            get { return _dg; }
        }

        // TODO: Allow shape to use a shading geometry.
        public DifferentialGeometry ShadingGeometry
        {
            get { return _dg; }
        }

		public Bsdf(DifferentialGeometry dg, Normal3D geometricNormal)
		{
			_dg = dg;
			_geometricNormal = geometricNormal;
			_bxdfs = new List<Bxdf>();

			_nn = dg.Normal;
			_sn = Vector3D.Normalize(dg.DpDu);
			_tn = Vector3D.Cross(_nn, _sn);
		}

		public void Add(Bxdf bxdf)
		{
			_bxdfs.Add(bxdf);
		}

		public ColorF Evaluate(Vector3D outgoingWorld, Vector3D incomingWorld,
                               BxdfType flags = BxdfType.All)
		{
			Vector3D outgoing = WorldToLocal(outgoingWorld);
			Vector3D incoming = WorldToLocal(incomingWorld);
			ColorF result = new ColorF();
			foreach (var bxdf in _bxdfs)
				result += bxdf.Evaluate(incoming, outgoing);
			return result;
		}

		private Vector3D WorldToLocal(Vector3D v)
		{
			return new Vector3D(
				Vector3D.Dot(v, _sn),
				Vector3D.Dot(v, _tn),
				Vector3D.Dot(v, _nn));
		}
	}
}