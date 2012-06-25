using Nexus;

namespace Aether.Shapes
{
	public class DifferentialGeometry
	{
		private readonly Point3D _point;
		private readonly Vector3D _dpDu;
		private readonly Vector3D _dpDv;
		private readonly Vector3D _dnDu;
		private readonly Vector3D _dnDv;
		private readonly float _u;
		private readonly float _v;
		private readonly Shape _shape;
		private readonly Normal3D _normal;

        public Point3D Point
        {
            get { return _point;}
        }

		public Normal3D Normal
		{
			get { return _normal; }
		}

		public Vector3D DpDu
		{
			get { return _dpDu; }
		}

		public DifferentialGeometry(Point3D point,
			Vector3D dpDu, Vector3D dpDv,
			Vector3D dnDu, Vector3D dnDv,
			float u, float v,
			Shape shape)
		{
			_point = point;
			_dpDu = dpDu;
			_dpDv = dpDv;
			_dnDu = dnDu;
			_dnDv = dnDv;
			_u = u;
			_v = v;
			_shape = shape;
			_normal = (Normal3D) Vector3D.Normalize(Vector3D.Cross(dpDu, dpDv));
		}

        public void ComputeDifferentials(RaySegment3D ray)
        {
            // TODO
        }
	}
}