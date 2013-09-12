namespace Aether.Sampling

open System.Collections.Generic
open Nexus


type Sample(imageX : int, imageY : int) =
    member this.ImageX = imageX
    member this.ImageY = imageY


[<AbstractClass>]
type Sampler(startPoint : IntPoint2D, endPoint : IntPoint2D) =
    abstract GetSamples : unit -> seq<Sample>


type RegularSampler(startPoint, endPoint) =
    inherit Sampler(startPoint, endPoint)

    let xStart = startPoint.X
    let yStart = startPoint.Y
    let xEnd = endPoint.X
    let yEnd = endPoint.Y

    override this.GetSamples () =
        let xPos = ref xStart
        let yPos = ref yStart

        seq {
            while !yPos < yEnd do
                yield Sample(!xPos, !yPos)

                xPos := !xPos + 1
                if !xPos = xEnd then
                    xPos := xStart
                    yPos := !yPos + 1
        }