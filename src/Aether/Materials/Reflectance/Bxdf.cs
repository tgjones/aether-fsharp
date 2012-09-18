using Nexus;
using Nexus.Graphics.Colors;

namespace Aether.Materials.Reflectance
{
	/// <summary>
	/// Base class for Brdf (Bi-directional Reflectance Distribution Function)
	/// and Btdf (Bi-directional Transmittance Distribution Function)
	/// </summary>
	public abstract class Bxdf
	{
		protected Bxdf(BxdfType type)
		{

		}

		public abstract ColorF Evaluate(Vector3D incoming, Vector3D outgoing);
	}
}