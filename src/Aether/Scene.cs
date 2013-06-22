﻿using Aether.Cameras;
using Aether.Integrators;
using Aether.Primitives;
using Aether.Sampling;
using System.Collections.Generic;
using Aether.Lights;
using Nexus.Graphics.Colors;

namespace Aether
{
	public class Scene
	{
		private readonly Camera _camera;
		private readonly SurfaceIntegrator _surfaceIntegrator;
		private readonly Sampler _sampler;
		private readonly Primitive _primitive;
        private readonly List<Light> _lights;

        public List<Light> Lights
        {
            get { return _lights; }
        }

		public Scene(Camera camera, SurfaceIntegrator surfaceIntegrator,
			Sampler sampler, Primitive primitive, List<Light> lights)
		{
			_camera = camera;
			_surfaceIntegrator = surfaceIntegrator;
			_sampler = sampler;
			_primitive = primitive;
            _lights = lights;
		}

		public bool TryIntersect(RaySegment3D ray, out Intersection intersection)
		{
			return _primitive.TryIntersect(ray, out intersection);
		}

        public bool Intersects(RaySegment3D ray)
        {
            return _primitive.Intersects(ray);
        }

		public void Render()
		{
			foreach (var sample in _sampler.GetSamples())
			{
				RaySegment3D ray = _camera.GenerateRay(sample);

				// Evaluate radiance along camera ray.
				var color = Li(ray, sample);

				_camera.Film.AddSample(sample, color);
			}
		}

		private ColorF Li(RaySegment3D ray, Sample sample)
		{
			return _surfaceIntegrator.Li(this, ray, sample);
		}
	}
}