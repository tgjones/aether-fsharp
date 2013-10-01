namespace Spectrum

open Xunit
open Xunit.Extensions
open FsUnit.Xunit
open Aether


[<AutoOpen>]
module SpectrumTestHelper =
    let checkSpectrumsEqual (s1 : Spectrum) (s2 : Spectrum) =
        s1.Coefficients.[0] |> should (equalWithin 0.01f) s2.Coefficients.[0]
        s1.Coefficients.[1] |> should (equalWithin 0.01f) s2.Coefficients.[1]
        s1.Coefficients.[2] |> should (equalWithin 0.01f) s2.Coefficients.[2]


type ``Given sampled data`` () =
    let wavelengths = [| 300.0f; 400.0f; 410.0f; 415.0f; 500.0f; 600.0f |]
    let values = [| 0.3f; 0.6f; 0.65f; 0.8f; 0.2f; 0.1f |]

    [<Fact>]
    let ``a Spectrum can be created`` () =
        let result = Spectrum.FromSampled wavelengths values
        result.Coefficients.[0] |> should (equalWithin 0.01f) 0.12f
        result.Coefficients.[1] |> should (equalWithin 0.01f) 0.12f
        result.Coefficients.[2] |> should (equalWithin 0.01f) 0.53f


type ``RGB <> XYZ conversion`` () =

    [<Theory>]
    [<InlineData(0.4f, 0.6f, 0.1f, 0.40f, 0.52f, 0.17f)>]
    let ``when RgbToXyz is called, it converts RGB data into XYZ data`` r g b expectedX expectedY expectedZ =
        let rgb = [| r; g; b |]
        let xyz = Spectrum.RgbToXyz(rgb)
        xyz.[0] |> should (equalWithin 0.01f) expectedX
        xyz.[1] |> should (equalWithin 0.01f) expectedY
        xyz.[2] |> should (equalWithin 0.01f) expectedZ
        
        let convertedBackRgb = Spectrum.XyzToRgb(xyz)
        convertedBackRgb.[0] |> should (equalWithin 0.01f) rgb.[0]
        convertedBackRgb.[1] |> should (equalWithin 0.01f) rgb.[1]
        convertedBackRgb.[2] |> should (equalWithin 0.01f) rgb.[2]

    [<Theory>]
    [<InlineData(0.3f, 0.8f, 0.5f, -0.51f, 1.23f, 0.38f)>]
    let ``when XyzToRgb is called, it converts XYZ data into RGB data`` x y z expectedR expectedG expectedB =
        let xyz = [| x; y; z |]
        let rgb = Spectrum.XyzToRgb(xyz)
        rgb.[0] |> should (equalWithin 0.01f) expectedR
        rgb.[1] |> should (equalWithin 0.01f) expectedG
        rgb.[2] |> should (equalWithin 0.01f) expectedB
        
        let convertedBackXyz = Spectrum.RgbToXyz(rgb)
        convertedBackXyz.[0] |> should (equalWithin 0.01f) xyz.[0]
        convertedBackXyz.[1] |> should (equalWithin 0.01f) xyz.[1]
        convertedBackXyz.[2] |> should (equalWithin 0.01f) xyz.[2]


type ``Given a spectrum`` () =
    let spectrum = Spectrum(0.3f, 0.4f, 0.5f)

    [<Fact>]
    let ``when Add is called, it sums the two spectrums`` () =
        spectrum.Add(Spectrum(0.1f, 0.2f, 0.3f))
        spectrum |> should equal (Spectrum(0.4f, 0.6f, 0.8f))

    [<Fact>]
    let ``the + operator adds values`` () =
        spectrum + Spectrum(0.1f, 0.2f, 0.3f) |> should equal (Spectrum(0.4f, 0.6f, 0.8f))

    [<Fact>]
    let ``the - operator subtracts values`` () =
        checkSpectrumsEqual (spectrum - Spectrum(0.1f, 0.2f, 0.3f)) (Spectrum(0.2f, 0.2f, 0.2f))

    [<Fact>]
    let ``the / operator divides values`` () =
        spectrum / Spectrum(0.1f, 0.2f, 5.0f) |> should equal (Spectrum(3.0f, 2.0f, 0.1f))
        spectrum / 2.0f |> should equal (Spectrum(0.15f, 0.2f, 0.25f))

    [<Fact>]
    let ``the * operator multiplies values`` () =
        checkSpectrumsEqual (spectrum * Spectrum(0.1f, 0.4f, 5.0f)) (Spectrum(0.03f, 0.16f, 2.5f))
        spectrum * 2.0f |> should equal (Spectrum(0.6f, 0.8f, 1.0f))

    [<Fact>]
    let ``Sqrt returns a new spectrum with square-rooted values`` () =
        checkSpectrumsEqual (Spectrum.Sqrt(spectrum)) (Spectrum(0.55f, 0.63f, 0.71f))

    [<Fact>]
    let ``Exp returns a new spectrum with exponentiated values`` () =
        checkSpectrumsEqual (Spectrum.Exp(spectrum)) (Spectrum(1.35f, 1.49f, 1.65f))

    [<Fact>]
    let ``ToXyz converts a spectrum to XYZ data`` () =
        let result = spectrum.ToXyz()
        result.[0] |> should (equalWithin 0.01f) 0.36f
        result.[1] |> should (equalWithin 0.01f) 0.39f
        result.[2] |> should (equalWithin 0.01f) 0.53f

type ``Given a non-black spectrum`` () =
    let spectrum = Spectrum(0.3f, 0.0f, 0.5f)

    [<Fact>]
    let ``IsBlack returns false`` () =
        spectrum.IsBlack() |> should be False

type ``Given a black spectrum`` () =
    let spectrum = Spectrum.Black()

    [<Fact>]
    let ``IsBlack returns true`` () =
        spectrum.IsBlack() |> should be True

type ``Given XYZ data`` () =
    let x, y, z = 0.1f, 0.5f, 0.3f

    [<Fact>]
    let ``FromXyz creates a spectrum from XYZ data`` () =
        checkSpectrumsEqual (Spectrum.FromXyz(x, y, z)) (Spectrum(-0.59f, 0.85f, 0.22f))