namespace Aether.Films

open System.Threading
open System.Windows.Media
open System.Windows.Media.Imaging
open Aether
open Aether.Math
open Aether.Parsing
open Aether.Sampling
open Aether.Filters


type FilmExtent = { XStart : int; XEnd : int;
                    YStart : int; YEnd : int }


[<AbstractClass>]
type Film(xRes, yRes) =
    let aspectRatio = (single xRes) / (single yRes)

    member this.XRes = xRes
    member this.YRes = yRes

    member this.AspectRatio = aspectRatio

    /// Updates the stored image with a given sample and corresponding radiance.
    /// The selected reconstruction filter will be applied.
    abstract AddSample : ICameraSample -> Spectrum -> unit

    /// Updates the stored image. Splatted values are summed, rather than
    /// a weighted average as is the case with AddSample.
    abstract Splat : ICameraSample -> Spectrum -> unit

    /// Allows the film to specify an extent for sampling that is different
    /// from the film resolution, in order to support sampling beyond the
    /// edges of the final image.
    abstract GetSampleExtent : unit -> FilmExtent

    /// Returns the range of pixels in the actual image.
    abstract GetPixelExtent : unit -> FilmExtent

    /// Notifies the film that a region of pixels has recently been updated.
    abstract UpdateDisplay : int -> int -> int -> int -> single -> unit

    /// Called when the film should do any processing necessary to create,
    /// and then display or store, the final image.
    abstract WriteImage : single -> unit


type private Pixel =
    val Lxyz      : single[]
    val WeightSum : ref<single>
    val SplatXyz  : single[]
    new() = { Lxyz = Array.zeroCreate 3;
              WeightSum = ref 0.0f; 
              SplatXyz = Array.zeroCreate 3 }


type CropWindow = { XMin : single; XMax : single;
                    YMin : single; YMax : single }


type ImageFilm(bitmap : WriteableBitmap, filter : Filter, cropWindow : CropWindow) =
    inherit Film(bitmap.PixelWidth, bitmap.PixelHeight)

    let xRes = bitmap.PixelWidth
    let yRes = bitmap.PixelHeight

    // Compute film image extent.
    let xPixelStart = ceil2int (single(xRes) * cropWindow.XMin)
    let xPixelCount = max 1 ((ceil2int (single(xRes) * cropWindow.XMax)) - xPixelStart)
    let yPixelStart = ceil2int (single(yRes) * cropWindow.YMin)
    let yPixelCount = max 1 ((ceil2int (single(yRes) * cropWindow.YMax)) - yPixelStart)

    // Allocate film image storage.
    let pixels = Array2D.init xPixelCount yPixelCount (fun x y -> Pixel())
    
    // Precompute filter weight table.
//    let filterTableSize = 16
//    let initFilterWeight x y =
//        let fx = (single(x) + 0.5f) * filter.XWidth / single(filterTableSize)
//        let fy = (single(y) + 0.5f) * filter.YWidth / single(filterTableSize)
//        filter.Evaluate fx fy
//    let filterTable = Array2D.init filterTableSize filterTableSize initFilterWeight

    // TODO: Test this!!!
    let atomicAdd (location1 : single byref) (value : single) = 
        let mutable initialValue = location1
        let mutable computedValue = initialValue + value
        while initialValue <> Interlocked.CompareExchange(&location1, computedValue, initialValue) do
            initialValue <- location1
            computedValue <- initialValue + value

    override this.AddSample sample l =
        // Compute sample's raster extent.
        let dimageX = sample.ImageX - 0.5f
        let dimageY = sample.ImageY - 0.5f
        let x0 = max (ceil2int  (dimageX - filter.XWidth)) xPixelStart
        let x1 = min (floor2int (dimageX + filter.XWidth)) (xPixelStart + xPixelCount - 1)
        let y0 = max (ceil2int  (dimageY - filter.YWidth)) yPixelStart
        let y1 = min (floor2int (dimageY + filter.YWidth)) (yPixelStart + yPixelCount - 1)

        if (x1 - x0 < 0) || (y1 - y0 < 0) then
            // Sample outside image extent
            ()
        else
            // Loop over filter support and add sample to pixel arrays.
            let xyz = l.ToXyz()

            for y = y0 to y1 do
                for x = x0 to x1 do
                    // Evaluate filter value at (x,y) pixel.
                    let filterWeight = filter.Evaluate (single(x) - dimageX)
                                                       (single(y) - dimageY)

                    // Update pixel values with filtered sample contribution.
                    let pixel = pixels.[x - xPixelStart, y - yPixelStart]
                    atomicAdd &pixel.Lxyz.[0] (filterWeight * xyz.[0])
                    atomicAdd &pixel.Lxyz.[1] (filterWeight * xyz.[1])
                    atomicAdd &pixel.Lxyz.[2] (filterWeight * xyz.[2])
                    atomicAdd &pixel.WeightSum.contents filterWeight

    override this.Splat sample l =
        let xyz = l.ToXyz()
        let x, y = floor2int sample.ImageX, floor2int sample.ImageY
        if x < xPixelStart || x - xPixelStart >= xPixelCount ||
           y < yPixelStart || y - yPixelStart >= yPixelCount then
           ()
        else
            let pixel = pixels.[x - xPixelStart, y - yPixelStart]
            atomicAdd &pixel.SplatXyz.[0] xyz.[0]
            atomicAdd &pixel.SplatXyz.[1] xyz.[1]
            atomicAdd &pixel.SplatXyz.[2] xyz.[2]

    override this.GetSampleExtent() =
        let xStart = floor2int (single(xPixelStart) + 0.5f - filter.XWidth)
        let xEnd   = ceil2int  (single(xPixelStart) + 0.5f + single(xPixelCount) + filter.XWidth)
        let yStart = floor2int (single(yPixelStart) + 0.5f - filter.YWidth)
        let yEnd   = ceil2int  (single(yPixelStart) + 0.5f + single(yPixelCount) + filter.YWidth)
        { XStart = xStart; XEnd = xEnd; YStart = yStart; YEnd = yEnd }

    override this.GetPixelExtent() =
        { XStart = xPixelStart; XEnd = xPixelStart + xPixelCount;
          YStart = yPixelStart; YEnd = yPixelStart + yPixelCount }

    override this.WriteImage splatScale =
        // Convert image to RGB and compute final pixel values.
        let mutable index = 0
        for y = 0 to yPixelCount - 1 do
            for x = 0 to xPixelCount - 1 do
                let pixel = pixels.[x, y]

                // Convert pixel XYZ color to RGB.
                let rgb = Spectrum.XyzToRgb pixel.Lxyz

                // Normalize pixel with weight sum.
                let weightSum = !pixel.WeightSum
                if weightSum <> 0.0f then
                    let invWeightSum = 1.0f / weightSum
                    rgb.[0] <- max 0.0f (rgb.[0] * invWeightSum)
                    rgb.[1] <- max 0.0f (rgb.[1] * invWeightSum)
                    rgb.[2] <- max 0.0f (rgb.[2] * invWeightSum)

                // Add splat value at pixel.
                let splatRgb = Spectrum.XyzToRgb pixel.SplatXyz
                rgb.[0] <- rgb.[0] + splatScale * splatRgb.[0]
                rgb.[1] <- rgb.[1] + splatScale * splatRgb.[1]
                rgb.[2] <- rgb.[2] + splatScale * splatRgb.[2]

                // Set pixel color in output bitmap.
                let color = Color.FromRgb(byte(rgb.[0] * 255.0f),
                                          byte(rgb.[1] * 255.0f),
                                          byte(rgb.[2] * 255.0f))
                bitmap.SetPixeli(index, color)
                index <- index + 1

    override this.UpdateDisplay x0 y0 x1 y1 splatScale =
        //bitmap.Invalidate()
        ()

    static member Create(parameters : ParamSet, filter : Filter) =
        //let filename = parameters.FindOneString "filename" ""
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
        let bitmap = WriteableBitmap(xres, yres, 96.0, 96.0, PixelFormats.Pbgra32, null)
        ImageFilm(bitmap, filter, cropWindow)