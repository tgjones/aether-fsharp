using Aether.Films;
using Nexus;

namespace Aether.Cameras
{
	public class OrthographicCamera : ProjectionCamera
	{
		public OrthographicCamera(Film film,
			float nearPlaneDistance, float farPlaneDistance,
			Vector3D lookDirection, Vector3D upDirection,
			Point3D position, float width)
			: base(film, new Nexus.Graphics.Cameras.OrthographicCamera
			{
				NearPlaneDistance = nearPlaneDistance,
				FarPlaneDistance = farPlaneDistance,
				LookDirection = lookDirection,
				UpDirection = upDirection,
				Position = position,
				Width = width
			})
		{

		}
	}
}