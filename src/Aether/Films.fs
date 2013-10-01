namespace Aether.Films

open System.Windows.Media
open System.Windows.Media.Imaging
open Aether
open Aether.Math
open Aether.Sampling


[<AbstractClass>]
type Film(xRes, yRes) =
    let aspectRatio = (single xRes) / (single yRes)

    member this.XRes = xRes
    member this.YRes = yRes

    member this.AspectRatio = aspectRatio

    abstract AddSample : Sample -> Spectrum -> unit


type WriteableBitmapFilm(bitmap : WriteableBitmap) =
    inherit Film(bitmap.PixelWidth, bitmap.PixelHeight)

    let xRes = bitmap.PixelWidth
    let yRes = bitmap.PixelHeight

    let surface = Array2D.zeroCreate xRes yRes

    override this.AddSample sample c =
        surface.[sample.ImageX, sample.ImageY] <- c

    member this.Present () =
        let spectrumToColor (s : Spectrum) =
            let xyz = s.ToXyz()
            let rgb = Spectrum.XyzToRgb(xyz)
            Color.FromRgb(byte(rgb.[0] * 255.0f),
                          byte(rgb.[1] * 255.0f),
                          byte(rgb.[2] * 255.0f))

        let mutable index = 0
        for y in 0..(yRes-1) do
            for x in 0..(xRes-1) do
                bitmap.SetPixeli(index, spectrumToColor surface.[x, y])
                index <- index + 1

        //bitmap.Invalidate()