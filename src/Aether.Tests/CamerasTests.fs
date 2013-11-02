namespace ``Cameras - Orthographic camera``
    open System.Windows.Media
    open System.Windows.Media.Imaging
    open Xunit
    open FsUnit.Xunit
    open Aether.Geometry
    open Aether.Transforms
    open Aether.Filters
    open Aether.Sampling
    open Aether.Films
    open Aether.Cameras

    type ``Given an orthographic camera`` () =
        let cam2World = Transform.LookAt(Point(0.0f, 0.0f, -1.0f), Point.Zero, Vector.UnitY)
        let filmCropWindow = { XMin = 0.0f; XMax = 1.0f; YMin = 0.0f; YMax = 1.0f; }
        let filmBitmap = WriteableBitmap(10, 10, 96.0, 96.0, PixelFormats.Pbgra32, null)
        let film = ImageFilm(filmBitmap, BoxFilter(1.0f, 1.0f), filmCropWindow)
        let cameraScreenWindow = { XMin = -1.0f; XMax = 1.0f; YMin = -1.0f; YMax = 1.0f; }
        let camera = OrthographicCamera(cam2World, cameraScreenWindow,
                                        0.0f, 1.0f, 0.0f, 0.0f, film)

        [<Fact>] 
        let ``when GenerateRay() is called, it creates a ray with the correct parameters``() =
            let sample = Sample(4.0f, 3.0f, 0.5f, 0.5f, 0.0f)

            let ray = camera.GenerateRay(sample)

            ray.Direction |> should equal (Vector.UnitZ)
            ray.MaxT |> should equal infinityf
            ray.MinT |> should equal 0.0f
            ray.Origin.X |> should (equalWithin 0.01f) -0.2f
            ray.Origin.Y |> should (equalWithin 0.01f) 0.4f
            ray.Origin.Z |> should (equalWithin 0.01f) 1.0f
            ray.Time |> should equal 0.0f