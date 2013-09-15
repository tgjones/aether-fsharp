namespace Aether.Filters

open System
open Nexus


[<AbstractClass>]
type Filter(xWidth, yWidth) =

    abstract Evaluate : single -> single -> single

    static member CalculateInverses xWidth yWidth =
        (1.0f / xWidth, 1.0f / yWidth)


type BoxFilter(xWidth, yWidth) =
    inherit Filter(xWidth, yWidth)

    override this.Evaluate x y = 1.0f


type GaussianFilter(xWidth, yWidth, alpha) =
    inherit Filter(xWidth, yWidth)

    let expX = MathUtility.Exp(-alpha * xWidth * xWidth)
    let expY = MathUtility.Exp(-alpha * yWidth * yWidth)

    let gaussian d expv =
        Math.Max(0.0f, MathUtility.Exp(-alpha * d * d) - expv)

    override this.Evaluate x y =
        (gaussian x expX) * (gaussian y expY)


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
        (mitchell1D x * inverseXWidth) * (mitchell1D y * inverseYWidth)


type LanczosSincFilter(xWidth, yWidth, tau) =
    inherit Filter(xWidth, yWidth)

    let inverseXWidth, inverseYWidth = Filter.CalculateInverses xWidth yWidth

    let sinc1D (x : single) =
        let x' = Math.Abs(x)
        if x < 1.0f ** -5.0f then 1.0f
        elif x > 1.0f then 0.0f
        else
            let x'' = x' * MathUtility.PI
            let sinc = MathUtility.Sin(x'') / x''
            let lanczos = MathUtility.Sin(x'' * tau) / (x'' * tau)
            sinc * lanczos

    override this.Evaluate x y =
        sinc1D(x * inverseXWidth) * sinc1D(y * inverseYWidth)


type TriangleFilter(xWidth, yWidth) =
    inherit Filter(xWidth, yWidth)

    override this.Evaluate x y =
        Math.Max(0.0f, xWidth - Math.Abs(x)) *
        Math.Max(0.0f, yWidth - Math.Abs(y))