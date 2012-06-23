using Nexus;
using Nexus.Graphics.Transforms;

namespace Aether.Lights
{
	public class DirectionalLight : Light
	{
		private readonly Vector3D _direction;
		private readonly ColorF _radiance;

		public DirectionalLight(Transform3D transform, Vector3D direction, ColorF radiance)
			: base(transform)
		{
			_direction = Vector3D.Normalize(transform.Transform(direction));
			_radiance = radiance;
		}

		public override ColorF Evaluate(Point3D point, out Vector3D directionToLight)
		{
			directionToLight = _direction;
			return _radiance;
		}
	}
}