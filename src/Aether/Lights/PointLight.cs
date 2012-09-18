using Nexus;
using Nexus.Graphics.Colors;
using Nexus.Graphics.Transforms;

namespace Aether.Lights
{
	public class PointLight : Light
	{
		private readonly Point3D _position;
		private readonly ColorF _intensity;

		public PointLight(Transform3D transform, ColorF intensity)
			: base(transform)
		{
			_position = transform.Transform(Point3D.Zero);
			_intensity = intensity;
		}

		public override ColorF Evaluate(Point3D point, out Vector3D directionToLight, out VisibilityTester visibilityTester)
		{
			Vector3D vectorToLight = _position - point;
			directionToLight = Vector3D.Normalize(vectorToLight);
            visibilityTester = new VisibilityTester(point, _position);
			return _intensity / vectorToLight.LengthSquared();
		}
	}
}