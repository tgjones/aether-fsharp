namespace ``Monte Carlo``

open System
open Xunit
open Xunit.Extensions
open FsUnit.Xunit
open Aether.MonteCarlo


type ``Global functions`` () =

    [<Theory>]
    [<InlineData(false, 0.1f, 0.3f, 0.5f, 0.7f, 0.9f)>]
    [<InlineData(true,  0.03f, 0.24f, 0.55f, 0.60f, 0.94f)>]
    let ``stratifiedSample1D`` jitter v1 v2 v3 v4 v5 =
        let rng = Random(1000)
        let samples = stratifiedSample1D 5 rng jitter
        checkListsRoughlyMatch samples [ v1; v2; v3; v4; v5 ]