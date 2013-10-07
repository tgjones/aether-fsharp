namespace Aether.Cameras

open Aether.Math
open Aether.Geometry
open Aether.Transforms
open Aether.Sampling
open Aether.Films


[<AbstractClass>]
type Camera(cam2World : Transform, 
            shutterOpen : single, shutterClose : single,
            film : Film) =
    member this.Film = film

    abstract GenerateRay : ICameraSample -> RaySegment


[<AbstractClass>]
type ProjectiveCamera(cam2World, projection : Transform,
                      screenWindow : single[], 
                      shutterOpen, shutterClose, 
                      lensRadius, focalDistance,
                      film) =
    inherit Camera(cam2World, shutterOpen, shutterClose, film)

    // Compute projective camera transformations.
    let cameraToScreen = projection

    // Compute projective camera screen transformations.
    let screenToRaster = Transform.Scale(single film.XRes, single film.YRes, 1.0f) *
                         Transform.Scale(1.0f / (screenWindow.[1] - screenWindow.[0]),
                                         1.0f / (screenWindow.[2] - screenWindow.[3]),
                                         1.0f) *
                         Transform.Translate(-screenWindow.[0], -screenWindow.[3], 0.0f)
    let rasterToScreen = Transform.Inverse screenToRaster
    let rasterToCamera = (Transform.Inverse cameraToScreen) * rasterToScreen

    member this.LensRadius = lensRadius
    member this.RasterToCamera = rasterToCamera


type OrthographicCamera(cam2World : Transform, screenWindow, shutterOpen, shutterClose,
                        lensRadius, focalDistance, film) =
    inherit ProjectiveCamera(cam2World, Transform.Orthographic(0.0f, 1.0f),
                             screenWindow, shutterOpen, shutterClose,
                             lensRadius, focalDistance, film)

    override this.GenerateRay sample =
        // Generate raster and camera samples.
        let rasterPoint = Point(single(sample.ImageX), single(sample.ImageY), 0.0f)
        let cameraPoint = this.RasterToCamera |>> rasterPoint
        let ray = RaySegment(cameraPoint, Vector(0.0f, 0.0f, 1.0f), 0.0f, infinityf)

        // TODO: Modify for depth of field.
//        if this.LensRadius > 0.0f then
//            // Sample point on lens.
//            let lensU, lensV = concentricSampleDisk sample.LensU

//        let ray' = RaySegment.withTime sample.Time ray

        // TODO: Problem seems to be here.
        let ray' = cam2World |>> ray
        ray'


type PerspectiveCamera(cam2World : Transform, screenWindow, shutterOpen, shutterClose,
                       lensRadius, focalDistance, fieldOfView, film) =
    inherit ProjectiveCamera(cam2World, Transform.Perspective(fieldOfView, 1e-2f, 1000.0f),
                             screenWindow, shutterOpen, shutterClose, lensRadius,
                             focalDistance, film)

    override this.GenerateRay sample =
        // Generate raster and camera samples.
        let rasterPoint = Point(single(sample.ImageX), single(sample.ImageY), 0.0f)
        let cameraPoint = this.RasterToCamera |>> rasterPoint
        let ray = RaySegment(Point.Zero, cameraPoint |> Point.ToVector |> Vector.Normalize,
                             0.0f, infinityf)
        // TODO: Modify ray for depth of field.
        let ray' = cam2World |>> ray
        ray'


type EnvironmentCamera(cam2World : Transform, shutterOpen, shutterClose, film) =
    inherit Camera(cam2World, shutterOpen, shutterClose, film)

    override this.GenerateRay sample =
        // Compute environment camera ray direction
        let theta = pi * single(sample.ImageY) / single(film.YRes)
        let phi = 2.0f * pi * single(sample.ImageX) / single(film.XRes)
        let dir = Vector(sin theta * cos phi,
                         cos theta,
                         sin theta * sin phi)
        let ray = RaySegment(Point.Zero, dir, 0.0f, infinityf) // sample.Time
        let ray' = cam2World |>> ray
        ray'