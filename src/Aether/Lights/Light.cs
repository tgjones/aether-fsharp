using Nexus;
using Nexus.Graphics.Colors;
using Nexus.Graphics.Transforms;

namespace Aether.Lights
{
	public abstract class Light
	{
		private readonly Transform3D _transform;

		protected Light(Transform3D transform)
		{
			_transform = transform;
		}

		/// <summary>
		/// Calculates the radiance arriving at the specified world-space point due to this light. 
		/// </summary>
		/// <param name="point"></param>
		/// <param name="directionToLight"></param>
		/// <returns></returns>
		public abstract ColorF Evaluate(Point3D point,
                                        out Vector3D directionToLight,
                                        out VisibilityTester visibilityTester);
	}
}