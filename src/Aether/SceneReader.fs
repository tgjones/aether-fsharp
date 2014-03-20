namespace Aether

open System.Collections.Generic
open System.IO

open Aether.Math
open Aether.Geometry
open Aether.Transforms
open Aether.Parsing
open Aether.Lights
open Aether.Shapes
open Aether.Textures
open Aether.Materials
open Aether.Primitives
open Aether.Accelerators
open Aether.Filters
open Aether.Sampling
open Aether.Integrators
open Aether.Films
open Aether.Cameras


[<AutoOpen>]
module Factories =

    type TextureParams(geomParams : ParamSet, materialParams : ParamSet, 
                       floatTextures : Dictionary<string, Texture<single>>,
                       spectrumTextures : Dictionary<string, Texture<Spectrum>>) =

        member this.FindBool name defaultValue =
            geomParams.FindBool name (materialParams.FindBool name defaultValue)

        member this.FindInt name defaultValue =
            geomParams.FindInt name (materialParams.FindInt name defaultValue)

        member this.FindPoint name defaultValue =
            geomParams.FindPoint name (materialParams.FindPoint name defaultValue)

        member this.FindSingle name defaultValue =
            geomParams.FindSingle name (materialParams.FindSingle name defaultValue)

        member this.FindSpectrum name defaultValue =
            geomParams.FindSpectrum name (materialParams.FindSpectrum name defaultValue)

        member this.FindString name defaultValue =
            geomParams.FindString name (materialParams.FindString name defaultValue)

        member this.GetSpectrumTexture(n, defaultValue) =
            let name = geomParams.FindTexture n
            let name' = if name = "" then materialParams.FindTexture n else name
            if name' <> "" then
                if spectrumTextures.ContainsKey(name') then
                    spectrumTextures.[name']
                else
                    failwithf "Couldn't find spectrum texture named '%s' for parameter '%s'" name' n
            else
                let value = geomParams.FindSpectrum n (materialParams.FindSpectrum n defaultValue)
                upcast ConstantTexture<Spectrum>(value)

        member this.GetFloatTexture(n, defaultValue) =
            let name = geomParams.FindTexture n
            let name' = if name = "" then materialParams.FindTexture n else name
            if name' <> "" then
                if floatTextures.ContainsKey(name') then
                    floatTextures.[name']
                else
                    failwithf "Couldn't find float texture named '%s' for parameter '%s'" name' n
            else
                let value = geomParams.FindSingle n (materialParams.FindSingle n defaultValue)
                upcast ConstantTexture<single>(value)

        member this.GetOptionalFloatTexture(n) =
            let name = geomParams.FindTexture n
            let name' = if name = "" then materialParams.FindTexture n else name
            if name' = "" then
                None
            else
                if floatTextures.ContainsKey(name') then
                    Some(floatTextures.[name'])
                else
                    failwithf "Couldn't find float texture named '%s' for parameter '%s'" name' n


    let makeCamera(implementationType, parameters : ParamSet, cam2World : Transform, film : Film) : Camera =
        match implementationType with
        | "perspective" ->
            let mutable shutterOpen = parameters.FindSingle "shutteropen" 0.0f
            let mutable shutterClose = parameters.FindSingle "shutterclose" 1.0f
            if shutterClose < shutterOpen then
                swap &shutterOpen &shutterClose
            let lensRadius = parameters.FindSingle "lensradius" 0.0f
            let focalDistance = parameters.FindSingle "focaldistance" 1e30f
            let frame = parameters.FindSingle "frameaspectratio" (single(film.XRes) / single(film.YRes))
            let screenWindow = parameters.FindSingles("screenwindow")
            let screen =
                if List.length screenWindow = 4 then
                    { XMin = screenWindow.[0];
                      XMax = screenWindow.[1];
                      YMin = screenWindow.[2];
                      YMax = screenWindow.[3] }
                else if frame > 1.f then
                    { XMin = -frame;
                      XMax =  frame;
                      YMin = -1.0f;
                      YMax = 1.0f }
                else
                    { XMin = -1.0f; 
                      XMax =  1.0f;
                      YMin = -1.0f / frame;
                      YMax =  1.0f / frame }
            let fov = parameters.FindSingle "fov" 90.0f
            //let halfFov = parameters.FindSingle "halffov" -1.0f

            upcast PerspectiveCamera(cam2World, screen,
                                     shutterOpen, shutterClose,
                                     lensRadius, focalDistance, fov, 
                                     film)

        | _ -> failwith "Not implemented"

    let makeAccelerator(implementationType, parameters : ParamSet, primitives) : Aggregate =
        match implementationType with
        | "grid" ->
            upcast GridAccelerator(primitives, parameters.FindBool "refineimmediately" false)
        | _ -> failwith "Not implemented"

    let makeFilter(name, parameters : ParamSet) : Filter =
        match name with
        | "box" -> 
            let xWidth = parameters.FindSingle "xwidth" 0.5f
            let yWidth = parameters.FindSingle "ywidth" 0.5f
            upcast BoxFilter(xWidth, yWidth)
        | "guassian" ->
            let xWidth = parameters.FindSingle "xwidth" 2.0f
            let yWidth = parameters.FindSingle "ywidth" 2.0f
            let alpha = parameters.FindSingle "alpha" 2.0f
            upcast GaussianFilter(xWidth, yWidth, alpha)
        | "mitchell" ->
            let xWidth = parameters.FindSingle "xwidth" 2.0f
            let yWidth = parameters.FindSingle "ywidth" 2.0f
            let b = parameters.FindSingle "B" (1.0f / 3.0f)
            let c = parameters.FindSingle "C" (1.0f / 3.0f)
            upcast MitchellFilter(xWidth, yWidth, b, c)
        | "sinc" ->
            let xWidth = parameters.FindSingle "xwidth" 4.0f
            let yWidth = parameters.FindSingle "ywidth" 4.0f
            let tau = parameters.FindSingle "tau" 3.0f
            upcast LanczosSincFilter(xWidth, yWidth, tau)
        | "triangle" ->
            let xWidth = parameters.FindSingle "xwidth" 2.0f
            let yWidth = parameters.FindSingle "ywidth" 2.0f
            upcast TriangleFilter(xWidth, yWidth)
        | x -> failwithf "Unknown filter: %s" x

    let makeFilm(name, parameters : ParamSet, filter) : Film =
        match name with
        | "image" ->
            let xres = parameters.FindInt "xresolution" 640
            let yres = parameters.FindInt "yresolution" 480
            let cr = parameters.FindSingles("cropwindow")
            let cropWindow =
                if List.length cr = 4 then
                    { XMin = clamp (min cr.[0] cr.[1]) 0.0f 1.0f;
                        XMax = clamp (max cr.[0] cr.[1]) 0.0f 1.0f;
                        YMin = clamp (min cr.[2] cr.[3]) 0.0f 1.0f;
                        YMax = clamp (max cr.[2] cr.[3]) 0.0f 1.0f; }
                else
                    { XMin = 0.0f; XMax = 1.0f; YMin = 0.0f; YMax = 1.0f }
            upcast ImageFilm(xres, yres, filter, cropWindow)
        | x -> failwithf "Unknown film: %s" x

    let makeSurfaceIntegrator(name, parameters : ParamSet) : SurfaceIntegrator =
        match name with
        | "whitted" ->
            let maxDepth = parameters.FindInt "maxdepth" 5
            upcast WhittedIntegrator(maxDepth)
        | x -> failwithf "Unknown surface integrator: %s" x

    let makeSampler(name, parameters : ParamSet, film : Film, camera : Camera) : Sampler =
        match name with
        | "stratified" ->
            let jitter = parameters.FindBool "jitter" true
            let xSamples = parameters.FindInt "xsamples" 2
            let ySamples = parameters.FindInt "ysamples" 2

            let sampleExtent = film.GetSampleExtent()

            upcast StratifiedSampler(sampleExtent.XStart, sampleExtent.XEnd,
                                     sampleExtent.YStart, sampleExtent.YEnd,
                                     xSamples, ySamples, jitter,
                                     camera.ShutterOpen, camera.ShutterClose)
        | x -> failwithf "Unknown sampler: %s" x

    let makeMaterial(name, material2World, parameters : TextureParams) : Material =
        match name with
        | "matte" ->
            let kd = parameters.GetSpectrumTexture("Kd", Spectrum(0.5f))
            let sigma = parameters.GetFloatTexture("sigma", 0.0f)
            let bumpMap = parameters.GetOptionalFloatTexture("bumpmap")
            upcast MatteMaterial(kd, sigma, bumpMap)
        | x -> failwithf "Unknown material: %s" x

    let makeShape(name, object2World, reverseOrientation, parameters : ParamSet) : Shape =
        match name with
        | "sphere" ->
            let radius = parameters.FindSingle "radius" 1.0f
            let zMin = parameters.FindSingle "zmin" -radius
            let zMax = parameters.FindSingle "zmax" radius
            let phiMax = parameters.FindSingle "phimax" 360.0f
            upcast Sphere(object2World, reverseOrientation, radius,
                          zMin, zMax, phiMax)
        | "cylinder" ->
            let radius = parameters.FindSingle "radius" 1.0f
            let zMin = parameters.FindSingle "zmin" -1.0f
            let zMax = parameters.FindSingle "zmax" 1.0f
            let phiMax = parameters.FindSingle "phimax" 360.0f
            upcast Cylinder(object2World, reverseOrientation, radius,
                            zMin, zMax, phiMax)
        | "disk" ->
            let height = parameters.FindSingle "height" 0.0f
            let radius = parameters.FindSingle "radius" 1.0f
            let innerRadius = parameters.FindSingle "innerradius" 0.0f
            let phiMax = parameters.FindSingle "phimax" 360.0f
            upcast Disk(object2World, reverseOrientation, height,
                        radius, innerRadius, phiMax)
        | x -> failwithf "Unknown shape: %s" x

    let makeLight(name, light2World, parameters : ParamSet) : Light =
        match name with
        | "point" ->
            let I = parameters.FindSpectrum "I" (Spectrum(1.0f))
            let scale = parameters.FindSpectrum "scale" (Spectrum(1.0f))
            let from = parameters.FindPoint "from" Point.Zero
            let l2w = Transform.Translate(from.ToVector()) * light2World
            upcast PointLight(l2w, I * scale)
        | "distant" ->
            let L = parameters.FindSpectrum "L" (Spectrum(1.0f))
            let scale = parameters.FindSpectrum "scale" (Spectrum(1.0f))
            let fromPoint = parameters.FindPoint "from" Point.Zero
            let toPoint = parameters.FindPoint "to" (Point(0.0f, 0.0f, 1.0f))
            let dir = fromPoint - toPoint
            upcast DistantLight(light2World, L * scale, dir)
        | x -> failwithf "Unknown light: %s" x


module SceneReader =

    type RenderOptions() =
        member val FilterName = "box" with get, set
        member val FilterParams = ParamSet([]) with get, set
        member val FilmName = "image" with get, set
        member val FilmParams = ParamSet([]) with get, set
        member val SamplerName = "lowdiscrepancy" with get, set
        member val SamplerParams = ParamSet([]) with get, set
        member val AcceleratorName = "bvh" with get, set
        member val AcceleratorParams = ParamSet([]) with get, set
        member val RendererName = "sampler" with get, set
        member val RendererParams = ParamSet([]) with get, set
        member val SurfaceIntegratorName = "directlighting" with get, set
        member val SurfaceIntegratorParams = ParamSet([]) with get, set
        member val VolumeIntegratorName = "emission" with get, set
        member val VolumeIntegratorParams = ParamSet([]) with get, set
        member val CameraName = "perspective" with get, set
        member val CameraParams = ParamSet([]) with get, set
        member val CameraToWorld = Aether.Transforms.Transform(Matrix4x4.Identity) with get, set
        member val Lights = List<Light>() with get, set
        member val Primitives = List<Primitive>() with get, set

        member this.MakeCamera() =
            let filter = makeFilter(this.FilterName, this.FilterParams)
            let film = makeFilm(this.FilmName, this.FilmParams, filter)
            makeCamera(this.CameraName, this.CameraParams, this.CameraToWorld, film)


    type GraphicsState() =
        let floatTextures = Dictionary<string, Texture<single>>()
        let spectrumTextures = Dictionary<string, Texture<Spectrum>>()
        let namedMaterials = Dictionary<string, Material>()

        member this.FloatTextures = floatTextures
        member this.SpectrumTextures = spectrumTextures
        member val MaterialParams = ParamSet([]) with get, set
        member val Material = "matte" with get, set
        member this.NamedMaterials = namedMaterials
        member val CurrentNamedMaterial = "" with get, set
        member val AreaLightParams = ParamSet([]) with get, set
        member val AreaLight = "" with get, set
        member val ReverseOrientation = false with get, set

        member this.CreateMaterial(parameters : ParamSet, transform) =
            let textureParams = TextureParams(parameters, this.MaterialParams, floatTextures, spectrumTextures)

            if this.CurrentNamedMaterial <> "" && namedMaterials.ContainsKey(this.CurrentNamedMaterial) then
                namedMaterials.[this.CurrentNamedMaterial]
            else
                makeMaterial(this.Material, transform, textureParams)


    [<CompiledName("Read")>]
    let read (reader : TextReader) =

        let fileText = reader.ReadToEnd()
        let parsedFile = Parser.parse(fileText)

        let transformStack = Stack<Transform>()
        let transform = ref (Transforms.Transform())

        let renderOptions = RenderOptions()

        let graphicsStateStack = Stack<GraphicsState>()
        let graphicsState = ref (GraphicsState())

        let processDirective = function
        | StandardDirective(StandardDirectiveType.Camera, name, parameters) -> 
            renderOptions.CameraName <- name
            renderOptions.CameraParams <- parameters
            renderOptions.CameraToWorld <- Transform.Inverse(!transform)
        | StandardDirective(StandardDirectiveType.Accelerator, name, parameters) ->
            renderOptions.AcceleratorName <- name
            renderOptions.AcceleratorParams <- parameters
        | StandardDirective(StandardDirectiveType.Film, name, parameters) ->
            renderOptions.FilmName <- name
            renderOptions.FilmParams <- parameters
        | StandardDirective(StandardDirectiveType.Sampler, name, parameters) ->
            renderOptions.SamplerName <- name
            renderOptions.SamplerParams <- parameters
        | StandardDirective(StandardDirectiveType.PixelFilter, name, parameters) ->
            renderOptions.FilterName <- name
            renderOptions.FilterParams <- parameters
        | StandardDirective(StandardDirectiveType.SurfaceIntegrator, name, parameters) ->
            renderOptions.SurfaceIntegratorName <- name
            renderOptions.SurfaceIntegratorParams <- parameters
        | StandardDirective(StandardDirectiveType.Material, name, parameters) ->
            (!graphicsState).Material <- name
            (!graphicsState).MaterialParams <- parameters
            (!graphicsState).CurrentNamedMaterial <- ""
        | StandardDirective(StandardDirectiveType.LightSource, name, parameters) ->
            let light = makeLight(name, !transform, parameters)
            renderOptions.Lights.Add(light)
        | StandardDirective(StandardDirectiveType.Shape, name, parameters) ->
            // TODO: Animation.
            // Create primitive for static shape.
            let obj2World = !transform
            let shape = makeShape(name, obj2World, (!graphicsState).ReverseOrientation, parameters)
            let material = (!graphicsState).CreateMaterial(parameters, obj2World)
            // TODO: Area light
            let primitive = GeometricPrimitive(shape, material)
            // TODO: Instancing
            renderOptions.Primitives.Add(primitive)
        | LookAt(eye, lookAt, up) ->
            transform := !transform * Transform.LookAt(eye, lookAt, up)
        | Translate(delta) ->
            transform := !transform * Transform.Translate(delta)
        | Rotate(angle, axis) ->
            transform := !transform * (Transform.Rotate angle axis)
        | Scale(values) ->
            transform := !transform * Transform.Scale(values)
        | WorldBegin ->
            transform := Transforms.Transform()
        | AttributeBegin ->
            graphicsStateStack.Push(!graphicsState)
            transformStack.Push(!transform)
        | AttributeEnd ->
            graphicsState := graphicsStateStack.Pop()
            transform := transformStack.Pop()
        | WorldEnd -> ()
        | x ->
            failwithf "Not implemented: %s" (x.ToString())

        parsedFile |> List.iter processDirective

        let camera = renderOptions.MakeCamera()
        let surfaceIntegrator = makeSurfaceIntegrator(renderOptions.SurfaceIntegratorName, renderOptions.SurfaceIntegratorParams)
        let sampler = makeSampler(renderOptions.SamplerName, renderOptions.SamplerParams, camera.Film, camera)
        let accelerator = makeAccelerator(renderOptions.AcceleratorName, renderOptions.AcceleratorParams,
                                          renderOptions.Primitives |> Seq.toList)

        Scene(renderOptions.MakeCamera(), surfaceIntegrator, sampler, accelerator, renderOptions.Lights)