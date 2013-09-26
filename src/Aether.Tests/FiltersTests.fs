namespace Filters

open Xunit.Extensions
open FsUnit.Xunit
open Aether.Filters

type ``Given a filter`` () =
    [<Theory; PropertyData("TestCases")>] 
    let ``when evaluate is called on filter, it returns the correct result`` (filter : Filter) x y (expectedResult : single) =
        filter.Evaluate x y |> should (equalWithin 0.001f) expectedResult

    static member TestCases : obj[][] =
        [|
            [| BoxFilter(2.0f, 3.0f); 0.5f; 2.0f; 1.0f |]

            [| GaussianFilter(2.0f, 2.0f, 30.0f); 0.0f; 0.0f; 1.0f |]
            [| GaussianFilter(2.0f, 2.0f, 0.5f); 1.0f; 0.5f; 0.352059126f |]
            [| GaussianFilter(2.0f, 2.0f, 0.1f); 2.0f; 2.0f; 0.0f |]
            [| GaussianFilter(2.0f, 2.0f, 0.1f); 1.0f; 2.0f; 0.0f |]

            [| MitchellFilter(2.0f, 2.0f, 1.0f / 3.0f, 1.0f / 3.0f); 0.0f; 0.0f; 0.790123582f |]
            [| MitchellFilter(2.0f, 2.0f, 1.0f / 3.0f, 1.0f / 3.0f); 0.2f; 0.2f; 0.669487596f |]
            [| MitchellFilter(1.0f, 1.0f, 1.0f / 3.0f, 1.0f / 3.0f); 1.0f; 1.0f; 0.0f |]

            [| LanczosSincFilter(2.0f, 2.0f, 3.0f); 0.0f; 0.0f; 1.0f |]
            [| LanczosSincFilter(2.0f, 2.0f, 3.0f); 0.2f; 0.2f; 0.712915421f |]
            [| LanczosSincFilter(1.0f, 1.0f, 3.0f); 1.0f; 1.0f; 0.0f |]

            [| TriangleFilter(1.0f, 1.0f); 0.0f; 0.0f; 1.0f |]
            [| TriangleFilter(1.0f, 1.0f); 0.5f; 0.0f; 0.5f |]
            [| TriangleFilter(1.0f, 1.0f); 0.5f; 0.5f; 0.25f |]
        |]