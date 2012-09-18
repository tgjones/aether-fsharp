using Aether.Primitives;
using Aether.Sampling;
using Aether.Util;
using Nexus;
using Aether.Materials.Reflectance;
using Aether.Lights;
using Nexus.Graphics.Colors;

namespace Aether.Integrators
{
	public class WhittedIntegrator : SurfaceIntegrator
	{
		private readonly int _maxDepth;
		private int _rayDepth;
		
		public WhittedIntegrator(int maxDepth)
		{
			_maxDepth = maxDepth;
		}

		public override ColorF Li(Scene scene, RaySegment3D ray, Sample sample)
		{
            ColorF result = new ColorF();

			// Search for ray-primitive intersection
			Intersection intersection;
			if (!scene.TryIntersect(ray, out intersection))
			{
				// Handle ray with no intersection
				// TODO: Iterate through lights to see what they contribute to this ray
				return ColorsF.Black;
			}

            // TODO: Initialize alpha for ray hit.

            // Compute emitted and reflected light at ray intersection point.
            // Evaluate BSDF at hit point.
            Bsdf bsdf = intersection.GetBsdf(ray);

            // Initialize common variables for Whitted integrator.
            Point3D p = bsdf.ShadingGeometry.Point;
            Normal3D n = bsdf.ShadingGeometry.Normal;
            Vector3D wo = -ray.Direction;

            // TODO: Compute emitted light if ray hit an area light source.

            // Add contribution of each light source.
            foreach (var light in scene.Lights)
            {
                Vector3D directionToLight;
                VisibilityTester visibilityTester;
                ColorF li = light.Evaluate(p, out directionToLight, out visibilityTester);

                if (li.IsEmpty()) // Early exit for no light.
                    continue;

                ColorF f = bsdf.Evaluate(wo, directionToLight);
                if (!f.IsEmpty() && visibilityTester.Unoccluded(scene))
                    result += f * li * Vector3D.AbsDot(directionToLight, n);
                        //* visibilityTester.Transmittance(scene);
            }

            --_rayDepth;
            return result;

            /*
        // Add contribution of each light source
        Vector wi;
        for (u_int i = 0; i < scene->lights.size(); ++i) {
            VisibilityTester visibility;
            Spectrum Li = scene->lights[i]->Sample_L(p, &wi, &visibility);
            if (Li.Black()) continue;
            Spectrum f = bsdf->f(wo, wi);
            if (!f.Black() && visibility.Unoccluded(scene))
                L += f * Li * AbsDot(wi, n) * visibility.Transmittance(scene);
        }
        if (rayDepth++ < maxDepth) {
            // Trace rays for specular reflection and refraction
            Spectrum f = bsdf->Sample_f(wo, &wi,
                BxDFType(BSDF_REFLECTION | BSDF_SPECULAR));
            if (!f.Black() && AbsDot(wi, n) > 0.f) {
                // Compute ray differential _rd_ for specular reflection
                RayDifferential rd(p, wi);
                rd.hasDifferentials = true;
                rd.rx.o = p + isect.dg.dpdx;
                rd.ry.o = p + isect.dg.dpdy;
                // Compute differential reflected directions
                Normal dndx = bsdf->dgShading.dndu * bsdf->dgShading.dudx +
                    bsdf->dgShading.dndv * bsdf->dgShading.dvdx;
                Normal dndy = bsdf->dgShading.dndu * bsdf->dgShading.dudy +
                    bsdf->dgShading.dndv * bsdf->dgShading.dvdy;
                Vector dwodx = -ray.rx.d - wo, dwody = -ray.ry.d - wo;
                float dDNdx = Dot(dwodx, n) + Dot(wo, dndx);
                float dDNdy = Dot(dwody, n) + Dot(wo, dndy);
                rd.rx.d = wi -
                          dwodx + 2 * Vector(Dot(wo, n) * dndx +
                          dDNdx * n);
                rd.ry.d = wi -
                          dwody + 2 * Vector(Dot(wo, n) * dndy +
                          dDNdy * n);
                L += scene->Li(rd, sample) * f * AbsDot(wi, n);
            }
            f = bsdf->Sample_f(wo, &wi,
                BxDFType(BSDF_TRANSMISSION | BSDF_SPECULAR));
            if (!f.Black() && AbsDot(wi, n) > 0.f) {
                // Compute ray differential _rd_ for specular transmission
                RayDifferential rd(p, wi);
                rd.hasDifferentials = true;
                rd.rx.o = p + isect.dg.dpdx;
                rd.ry.o = p + isect.dg.dpdy;

                float eta = bsdf->eta;
                Vector w = -wo;
                if (Dot(wo, n) < 0) eta = 1.f / eta;

                Normal dndx = bsdf->dgShading.dndu * bsdf->dgShading.dudx + bsdf->dgShading.dndv * bsdf->dgShading.dvdx;
                Normal dndy = bsdf->dgShading.dndu * bsdf->dgShading.dudy + bsdf->dgShading.dndv * bsdf->dgShading.dvdy;

                Vector dwodx = -ray.rx.d - wo, dwody = -ray.ry.d - wo;
                float dDNdx = Dot(dwodx, n) + Dot(wo, dndx);
                float dDNdy = Dot(dwody, n) + Dot(wo, dndy);

                float mu = eta * Dot(w, n) - Dot(wi, n);
                float dmudx = (eta - (eta*eta*Dot(w,n))/Dot(wi, n)) * dDNdx;
                float dmudy = (eta - (eta*eta*Dot(w,n))/Dot(wi, n)) * dDNdy;

                rd.rx.d = wi + eta * dwodx - Vector(mu * dndx + dmudx * n);
                rd.ry.d = wi + eta * dwody - Vector(mu * dndy + dmudy * n);
                L += scene->Li(rd, sample) * f * AbsDot(wi, n);
            }
        }
        --rayDepth;
             * */
		}
	}
}