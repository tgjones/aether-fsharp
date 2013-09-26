namespace Aether.Sampling

open System.Collections.Generic


type Sample(imageX : int, imageY : int) =
    member this.ImageX = imageX
    member this.ImageY = imageY


[<AbstractClass>]
type Sampler(xStart : int, xEnd : int, yStart : int, yEnd : int) =
    abstract GetSamples : unit -> seq<Sample>


type RegularSampler(xStart, xEnd, yStart, yEnd) =
    inherit Sampler(xStart, xEnd, yStart, yEnd)

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