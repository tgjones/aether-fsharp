namespace ``Monte Carlo``

open System
open Xunit
open Xunit.Extensions
open FsUnit.Xunit
open Aether


type ``Global functions`` () =

    let rng = Random(1000)

    [<Theory>]
    [<PropertyData("stratifiedSampler1DTestData")>]
    let ``stratifiedSample1D`` jitter (expectedSampleValues : single list) =
        let samples = MonteCarlo.stratifiedSample1D (List.length expectedSampleValues) rng jitter
        samples |> List.iter2 (fun x y -> x |> should (equalWithin 0.01f) y) expectedSampleValues

    [<Theory>]
    [<PropertyData("stratifiedSampler2DTestData")>]
    let ``stratifiedSample2D`` jitter (expectedSampleValues : (single * single) list) =
        let samples = MonteCarlo.stratifiedSample2D 3 2 rng jitter
        let checkTuple (x1, y1) (x2, y2) =
            x1 |> should (equalWithin 0.01f) x2
            y1 |> should (equalWithin 0.01f) y2
        List.iter2 checkTuple samples expectedSampleValues

    static member stratifiedSampler1DTestData =
        seq<obj[]> {
            yield [| false; [ 0.1f; 0.3f; 0.5f; 0.7f; 0.9f ] |]
            yield [| true;  [ 0.03f; 0.24f; 0.55f; 0.60f; 0.94f ] |]
        }

    static member stratifiedSampler2DTestData =
        seq<obj[]> {
            yield [| false; [ (0.17f, 0.25f); (0.5f, 0.25f); (0.83f, 0.25f);
                              (0.17f, 0.75f); (0.5f, 0.75f); (0.83f, 0.75f) ] |]
            yield [| true;  [ (0.05f, 0.12f); (0.59f, 0.00f); (0.90f, 0.23f);
                              (0.32f, 0.56f); (0.56f, 0.51f); (0.67f, 0.59f) ] |]
        }