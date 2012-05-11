using Aether.Films;
using Aether.Sampling;
using Nexus;
using Nexus.Graphics;

namespace Aether.Cameras
{
	public abstract class ProjectionCamera : Camera
	{
		private readonly Matrix3D _projection;
		private readonly Matrix3D _view;

		protected ProjectionCamera(Film film, Nexus.Graphics.Cameras.ProjectionCamera nexusCamera)
			: base(film)
		{
			_projection = nexusCamera.GetProjectionMatrix(film.AspectRatio);
			_view = nexusCamera.GetViewMatrix();
		}

		public override Ray3D GenerateRay(Sample sample)
		{
			var viewport = new Viewport(0, 0, Film.XRes, Film.YRes);

			Point3D near = Unproject(viewport, sample.ImageX, sample.ImageY, viewport.MinDepth);
			Point3D far = Unproject(viewport, sample.ImageX, sample.ImageY, viewport.MaxDepth);

			return new Ray3D(near, Vector3D.Normalize(far - near));
		}

		private Point3D Unproject(Viewport viewport, int x, int y, float z)
		{
			return viewport.Unproject(new Point3D(x, y, z), _projection, _view, Matrix3D.Identity);
		}
	}
}