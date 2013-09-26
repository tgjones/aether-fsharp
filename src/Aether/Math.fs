namespace Aether.Math

open Nexus
open Nexus.Objects3D


[<AutoOpen>]
module GlobalFunctions =

    /// PI
    let pi = single(System.Math.PI)

    /// 1 / PI
    let invPi = 1.0f / pi

    /// 1 / (2 * PI)
    let invTwoPi = 1.0f / (2.0f * pi)

    /// 1 / (4 * PI)
    let invFourPi = 1.0f / (4.0f * pi)

    /// Linearly interpolates between two values.
    let inline lerp t v1 v2 = 
        (1.0f - t) * v1 + (t * v2)

    /// Restricts a value to lie between a lower and higher bound.
    let inline clamp value low high =
        if value < low then low
        else if value > high then high
        else value

    /// Converts an angle in degrees to an angle in radians.
    let inline toRadians degrees =
        pi / 180.0f * degrees

    /// Converts an angle in radians to an angle in degrees.
    let inline toDegrees radians =
        180.0f / pi * radians

    /// 1 / (log 2)
    let invLog2 = 1.0f / (log 2.0f)

    /// Calculates the binary logarithm of x.
    /// The binary logarithm of x is the power to which the number 2 must be
    /// raised to obtain the value x.
    let inline log2 x =
        (log x) * invLog2

    /// Floor of the given number, cast to an integer.
    let inline floor2int value =
        int(floor value)

    /// Binary logarithm of the given number, floored to an integer.
    let inline log2int value =
        floor2int (log2 value)
    
    /// Returns true if the given number is a power of 2.
    let inline isPowerOf2 value =
        value &&& (value - 1) = 0

    /// Swaps the left and right byref values.
    let inline swap (left : 'a byref) (right : 'a byref) =
        let temp = left
        left <- right
        right <- temp

    /// Solves a quadratic equation where a, b and c are the coefficients.
    /// (Quadratic equations have the form ax^2 + bx + c = 0.)
    let inline quadratic a b c =
        // Find quadratic discriminant.
        let discrim = b * b - 4.0f * a * c

        if discrim < 0.0f then (None, None)
        else
            let rootDiscrim = sqrt discrim

            // Compute quadratic t values.
            let q = if b < 0.0f then -0.5f * (b - rootDiscrim)
                    else -0.5f * (b + rootDiscrim)
            let mutable t0 = q / a
            let mutable t1 = c / q
            if t0 > t1 then swap &t0 &t1
            (Some(t0), Some(t1))


[<AutoOpen>]
module Extensions =
    type Nexus.Graphics.Colors.Spectrum with
        member this.ToColorRgbF () =
            let xyzToRgb (xyz : single[]) (rgb : single[]) =
                rgb.[0] <-  3.240479f*xyz.[0] - 1.537150f*xyz.[1] - 0.498535f*xyz.[2]
                rgb.[1] <- -0.969256f*xyz.[0] + 1.875991f*xyz.[1] + 0.041556f*xyz.[2]
                rgb.[2] <-  0.055648f*xyz.[0] - 0.204043f*xyz.[1] + 1.057311f*xyz.[2]

            let xyz = Array.create 3 0.0f
            this.Xyz(xyz)

            let rgb = Array.create 3 0.0f
            xyzToRgb xyz rgb

            Nexus.Graphics.Colors.ColorRgbF(rgb.[0], rgb.[1], rgb.[2])