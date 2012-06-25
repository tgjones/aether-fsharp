using Aether.Films;
using Aether.Sampling;
using Nexus;

namespace Aether.Cameras
{
    public abstract class Camera
    {
        private readonly Film _film;

    	public Film Film
    	{
			get { return _film; }
    	}

        protected Camera(Film film)
        {
            _film = film;
        }

        public abstract RaySegment3D GenerateRay(Sample sample);
    }
}