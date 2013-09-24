namespace Aether.Films

open Nexus.Graphics
open Nexus.Graphics.Colors
open Aether.Math
open Aether.Sampling


[<AbstractClass>]
type Film(xRes, yRes) =
    let aspectRatio = (single xRes) / (single yRes)

    member this.XRes = xRes
    member this.YRes = yRes

    member this.AspectRatio = aspectRatio

    abstract AddSample : Sample -> Spectrum -> unit


type ColorSurfaceFilm(xRes, yRes, multiSampleCount) =
    inherit Film(xRes, yRes)

    let surface = new ColorSurface(xRes, yRes, multiSampleCount)

    override this.AddSample sample c =
        surface.[sample.ImageX, sample.ImageY, 0] <- ColorF(c.ToColorRgbF(), 1.0f)

    member this.Present imageBuffer =
        surface.Resolve(imageBuffer)