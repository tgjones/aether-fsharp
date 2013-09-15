namespace Aether.Cameras

open Nexus
open Nexus.Graphics
open Aether.Math
open Aether.Sampling
open Aether.Films


[<AbstractClass>]
type Camera(film : Film) =
    member this.Film = film

    abstract GenerateRay : Sample -> RaySegment3D


type ProjectionCamera(film, nexusCamera : Nexus.Graphics.Cameras.ProjectionCamera) =
    inherit Camera(film)

    let projection = nexusCamera.GetProjectionMatrix(film.AspectRatio)
    let view = nexusCamera.GetViewMatrix()

    override this.GenerateRay sample =
        let viewport = Viewport3D(0, 0, film.XRes, film.YRes)

        let unproject (viewport : Viewport3D) (x : int) (y : int) z =
            viewport.Unproject(Point3D(single x, single y, z),
                               projection, view, 
                               Matrix3D.Identity)

        let near = unproject viewport sample.ImageX sample.ImageY viewport.MinDepth
        let far = unproject viewport sample.ImageX sample.ImageY viewport.MaxDepth

        let nearToFar = far - near
        RaySegment3D(near, Vector3D.Normalize(nearToFar),
                     0.0f, nearToFar.Length())


type OrthographicCamera(film, nearPlaneDistance, farPlaneDistance,
                        lookDirection, upDirection, 
                        position, width) =
    inherit ProjectionCamera(film, 
        new Nexus.Graphics.Cameras.OrthographicCamera 
            (NearPlaneDistance = nearPlaneDistance,
            FarPlaneDistance = farPlaneDistance,
            LookDirection = lookDirection,
            UpDirection = upDirection,
            Position = position,
            Width = width))


type PerspectiveCamera(film, nearPlaneDistance, farPlaneDistance,
                       lookDirection, upDirection, 
                       position, fieldOfView) =
    inherit ProjectionCamera(film, 
        new Nexus.Graphics.Cameras.PerspectiveCamera 
            (NearPlaneDistance = nearPlaneDistance,
            FarPlaneDistance = farPlaneDistance,
            LookDirection = lookDirection,
            UpDirection = upDirection,
            Position = position,
            FieldOfView = fieldOfView))


type EnvironmentCamera(film) =
    inherit Camera(film)

    override this.GenerateRay sample =
        // Compute environment camera ray direction
        let theta = MathUtility.PI * single(sample.ImageY) / single(film.YRes)
        let phi = 2.0f * MathUtility.PI * single(sample.ImageX) / single(film.XRes)
        let dir = Vector3D(MathUtility.Sin(theta) * MathUtility.Cos(phi),
                           MathUtility.Cos(theta),
                           MathUtility.Sin(theta) * MathUtility.Sin(phi))
        RaySegment3D(Point3D.Zero, dir)