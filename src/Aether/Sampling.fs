namespace Aether.Sampling

open System.Collections.Generic
open Aether.Math
open Aether.MonteCarlo

type ICameraSample =
    abstract ImageX : single
    abstract ImageY : single
    abstract LensU : single
    abstract LensV : single
    abstract Time : single


type Sample(imageX, imageY, lensU, lensV, time) =
    member this.ImageX = imageX
    member this.ImageY = imageY

    interface ICameraSample with
        member this.ImageX = imageX
        member this.ImageY = imageY
        member this.LensU = lensU
        member this.LensV = lensV
        member this.Time = time


[<AbstractClass>]
type Sampler(xStart : int, xEnd : int, yStart : int, yEnd : int, samplesPerPixel : int,
             shutterOpen : single, shutterClose : single) =
    abstract GetSamples : System.Random -> seq<Sample>


type StratifiedSampler(xStart, xEnd, yStart, yEnd,
                       xPixelSamples, yPixelSamples, jitterSamples,
                       shutterOpen, shutterClose) =
    inherit Sampler(xStart, xEnd, yStart, yEnd,
                    xPixelSamples * yPixelSamples,
                    shutterOpen, shutterClose)

    let sampleBuffer = Array.zeroCreate (5 * xPixelSamples * yPixelSamples)

    override this.GetSamples rng =
        let xPos = ref xStart
        let yPos = ref yStart

        let numSamples = xPixelSamples * yPixelSamples

        seq {
            while !yPos < yEnd do
                // Generate stratified samples, and shift image samples to correct coordinates.
                let imageSamples = stratifiedSample2D xPixelSamples yPixelSamples rng jitterSamples
                                   |> List.map (fun (x, y) -> (x + single(!xPos), y + single(!yPos)))
                let lensSamples  = stratifiedSample2D xPixelSamples yPixelSamples rng jitterSamples
                                   |> shuffle rng // Decorrelate sample dimensions.
                let timeSamples  = stratifiedSample1D (xPixelSamples * yPixelSamples) rng jitterSamples
                                   |> shuffle rng // Decorrelate sample dimensions.

                for i = 0 to numSamples - 1 do
                    let (imageX, imageY) = List.nth imageSamples i
                    let (lensU, lensV) = List.nth lensSamples i
                    let time = lerp (List.nth timeSamples i) shutterOpen shutterClose
                    yield Sample(imageX, imageY, lensU, lensV, time)

                xPos := !xPos + 1
                if !xPos = xEnd then
                    xPos := xStart
                    yPos := !yPos + 1
        }