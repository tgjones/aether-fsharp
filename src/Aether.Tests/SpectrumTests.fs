namespace Spectrum

open Xunit
open Xunit.Extensions
open FsUnit.Xunit
open Aether


type ``Given sampled data`` () =
    let wavelengths = [| 300.0f; 400.0f; 410.0f; 415.0f; 500.0f; 600.0f |]
    let values = [| 0.3f; 0.6f; 0.65f; 0.8f; 0.2f; 0.1f |]

    [<Fact>]
    let ``a Spectrum can be created`` () =
        let result = RgbSpectrum.FromSampled wavelengths values
        result.Coefficients.[0] |> should (equalWithin 0.01f) 0.12f
        result.Coefficients.[1] |> should (equalWithin 0.01f) 0.12f
        result.Coefficients.[2] |> should (equalWithin 0.01f) 0.53f


type ``Given a spectrum`` () =
    let spectrum = RgbSpectrum([| 0.3f; 0.4f; 0.5f |])

    [<Fact>]
    let ``arithmetic operators work without modifying original values`` () =
        spectrum + spectrum |> should equal (RgbSpectrum([| 0.6f; 0.8f; 1.0f |]))
        spectrum |> should equal (RgbSpectrum([| 0.3f; 0.4f; 0.5f |]))