namespace Aether.Filters

open System
open Aether.Math


/// Filters are used by the <see cref="Film" /> class to filter output before 
/// writing it to disk. Filters have a width, which may be different in the x
/// and y directions. The overall extent of the filter is twice the 
/// corresponding width.
[<AbstractClass>]
type Filter(xWidth, yWidth) =

    /// Returns the weight of the sample for the given coordinates. The 
    /// coordinates specify the sample point relative to the filter's
    /// centre.
    abstract Evaluate : single -> single -> single

    member this.XWidth = xWidth
    member this.YWidth = yWidth

    static member CalculateInverses xWidth yWidth =
        (1.0f / xWidth, 1.0f / yWidth)


/// The box filter weights all samples equally. This is not a good filter.
type BoxFilter(xWidth, yWidth) =
    inherit Filter(xWidth, yWidth)

    override this.Evaluate x y = 1.0f


/// The gaussian filter is better than the box and triangle filters. It 
/// evaluates to 0 at the extents. Between those limits, it falls off using
/// a Gaussian curve. The Gaussian filter causes slight blurring.
type GaussianFilter(xWidth, yWidth, alpha) =
    inherit Filter(xWidth, yWidth)

    let expX = exp (-alpha * xWidth * xWidth)
    let expY = exp (-alpha * yWidth * yWidth)

    let gaussian d expv =
        max 0.0f ((exp (-alpha * d * d)) - expv)

    override this.Evaluate x y =
        (gaussian x expX) * (gaussian y expY)


/// The Mitchell filter was created by Mitchell and Netravali (1988). It 
/// provides crisper results than the Gaussian filter, while still accurately
/// representing the function being sampled. It has two parameters called B and C;
/// these should lie along the line B + 2C = 1.
type MitchellFilter(xWidth, yWidth, b, c) =
    inherit Filter(xWidth, yWidth)

    let inverseXWidth, inverseYWidth = Filter.CalculateInverses xWidth yWidth

    let mitchell1D x =
        let x' = Math.Abs(2.0f * x)
        if x' > 1.0f then
            ((-b - 6.0f * c) * x' * x' * x' + (6.0f * b + 30.0f * c) * x' * x' +
             (-12.0f * b - 48.0f * c) * x' + (8.0f * b + 24.0f * c)) * (1.0f / 6.0f)
        else
            ((12.0f - 9.0f * b - 6.0f * c) * x' * x' * x' +
             (-18.0f + 12.0f * b + 6.0f * c) * x' * x' +
             (6.0f - 2.0f * b)) * (1.0f / 6.0f)

    override this.Evaluate x y =
        (mitchell1D (x * inverseXWidth)) * (mitchell1D (y * inverseYWidth))


/// The Lanczos filter is based on the sinc function. The tau parameter controls
/// how many sinc function cycles are evaluated before it is clamped to a value
/// of 0.
type LanczosSincFilter(xWidth, yWidth, tau) =
    inherit Filter(xWidth, yWidth)

    let inverseXWidth, inverseYWidth = Filter.CalculateInverses xWidth yWidth

    let sinc1D (x : single) =
        let x' = Math.Abs(x)
        if x < 10.0f ** -5.0f then 1.0f
        elif x > 1.0f then 0.0f
        else
            let x'' = x' * pi
            let sinc = sin x'' / x''
            let lanczos = (sin (x'' * tau)) / (x'' * tau)
            sinc * lanczos

    override this.Evaluate x y =
        sinc1D(x * inverseXWidth) * sinc1D(y * inverseYWidth)


/// The triangle filter is slightly better than the box filter. Samples at the
/// centre point have a weight of 1, falling off linearly to the square extent
/// of the filter.
type TriangleFilter(xWidth, yWidth) =
    inherit Filter(xWidth, yWidth)

    override this.Evaluate x y =
        Math.Max(0.0f, xWidth - Math.Abs(x)) *
        Math.Max(0.0f, yWidth - Math.Abs(y))