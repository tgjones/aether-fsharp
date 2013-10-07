module Aether.MonteCarlo

open Aether.Math
open Aether.Geometry


/// Smallest floating point value less than one; all canonical random samples
/// should be <= this.
let oneMinusEpsilon = 0.9999999403953552f


let stratifiedSample1D count (rng : System.Random) jitter =
    let invTot = 1.0f / single(count)
    [ for i = 0 to count - 1 do
        let delta = if jitter then rng.NextSingle() else 0.5f
        yield min ((single(i) + delta) * invTot) oneMinusEpsilon ]

let stratifiedSample2D nx ny (rng : System.Random) jitter =
    let dx, dy = 1.0f / single(nx), 1.0f / single(ny)
    [ for y in 0 .. (ny-1) do
        for x in 0 .. (nx-1) do
            let jx = if jitter then rng.NextSingle() else 0.5f
            let jy = if jitter then rng.NextSingle() else 0.5f
            let tempX = min ((single(x) + jx) * dx) oneMinusEpsilon
            let tempY = min ((single(y) + jy) * dy) oneMinusEpsilon
            yield (tempX, tempY) ]

let shuffle (rng : System.Random) samples =
    let samplesArray = samples |> List.toArray
    let count = samples |> List.length
    for i = 0 to count - 1 do
        let other = i + (rng.Next() % (count - i))
        swap &samplesArray.[i] &samplesArray.[other]
    samplesArray |> Array.toList

let latinHypercube (samples : single[]) nDim (rng : System.Random) =
    // Generate LHS samples along diagonal.
    let delta = 1.0f / single(samples.Length)
    for i = 0 to samples.Length-1 do
        for j = 0 to nDim-1 do
            samples.[nDim * i + j] <- min ((single(i) + rng.NextSingle()) * delta)
                                          oneMinusEpsilon

    // Permute LHS samples in each dimension.
    for i = 0 to nDim-1 do
        for j = 0 to samples.Length-1 do
            let other = j + (rng.Next() % (samples.Length - j))
            swap &samples.[nDim * j + i] &samples.[nDim * other + i]

let uniformSampleHemisphere u1 u2 =
    let z = u1
    let r = sqrt (max 0.0f (1.0f - z * z))
    let phi = 2.0f * pi * u2
    let x = r * cos phi
    let y = r * sin phi
    Vector(x, y, z)

let uniformHemispherePdf () = invTwoPi

let uniformSampleSphere u1 u2 =
    let z = 1.0f - 2.0f * u1
    let r = sqrt (max 0.0f (1.0f - z * z))
    let phi = 2.0f * pi * u2
    let x = r * cos phi
    let y = r * sin phi
    Vector(x, y, z)

let uniformSpherePdf () = 1.0f / (4.0f * pi)

let uniformSampleDisk u1 u2 =
    let r = sqrt u1
    let theta = 2.0f * pi * u2
    let x = r * cos theta
    let y = r * sin theta
    (x, y)

let concentricSampleDisk u1 u2 =
    // Map uniform random numbers to [-1,1].
    let sx = 2.0f * u1 - 1.0f
    let sy = 2.0f * u2 - 1.0f

    // Map square to (r,theta)

    // Handle degeneracy at the origin.
    if sx = 0.0f && sy = 0.0f then (0.0f, 0.0f)
    else
        let calculate () =
            if sx >= -sy then
                if sx > sy then
                    // Handle first region of disk.
                    let theta = if sy > 0.0f then sy / sx
                                else 8.0f + sy / sx
                    (sx, theta)
                else
                    // Handle second region of disk.
                    (sy, 2.0f - sx / sy)
            else
                if sx <= sy then
                    // Handle third region of disk.
                    (-sx, 4.0f - sy / -sx)
                else
                    // Handle fourth region of disk.
                    (-sy, 6.0f + sx / -sy)

        let r, theta = calculate()
        let theta' = theta * pi / 4.0f
        let dx = r * cos theta
        let dy = r * sin theta
        (dx, dy)

