using Aether.Films;
using Nexus;

namespace Aether.Cameras
{
	public class PerspectiveCamera : ProjectionCamera
	{
		public PerspectiveCamera(Film film,
			float nearPlaneDistance, float farPlaneDistance,
			Vector3D lookDirection, Vector3D upDirection,
			Point3D position, float fieldOfView)
			: base(film, new Nexus.Graphics.Cameras.PerspectiveCamera
			{
				NearPlaneDistance = nearPlaneDistance,
				FarPlaneDistance = farPlaneDistance,
				LookDirection = lookDirection,
				UpDirection = upDirection,
				Position = position,
				FieldOfView = fieldOfView
			})
		{

		}
	}
}