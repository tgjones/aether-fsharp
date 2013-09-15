module Aether.Tests.FiltersTests

open NUnit.Framework
open Nexus
open Aether.Filters


[<TestCaseSource("testCases")>] 
let ``Filter Evaluate() works`` (filter : Filter) x y (expectedResult : single) =
    let result = filter.Evaluate x y
    Assert.That(result, Is.EqualTo(expectedResult).Within(0.001f))

let oneThird = 1.0f / 3.0f
let testCases =
    [
        TestCaseData(BoxFilter(2.0f, 3.0f), 0.5f, 2.0f, 1.0f)

        TestCaseData(GaussianFilter(2.0f, 2.0f, 30.0f), 0.0f, 0.0f, 1.0f)
        TestCaseData(GaussianFilter(2.0f, 2.0f, 0.5f), 1.0f, 0.5f, 0.352059126f)
        TestCaseData(GaussianFilter(2.0f, 2.0f, 0.1f), 2.0f, 2.0f, 0.0f)
        TestCaseData(GaussianFilter(2.0f, 2.0f, 0.1f), 1.0f, 2.0f, 0.0f)

        TestCaseData(MitchellFilter(2.0f, 2.0f, oneThird, oneThird), 0.0f, 0.0f, 0.790123582f)
        TestCaseData(MitchellFilter(2.0f, 2.0f, oneThird, oneThird), 0.2f, 0.2f, 0.669487596f)
        TestCaseData(MitchellFilter(1.0f, 1.0f, oneThird, oneThird), 1.0f, 1.0f, 0.0f)

        TestCaseData(LanczosSincFilter(2.0f, 2.0f, 3.0f), 0.0f, 0.0f, 1.0f)
        TestCaseData(LanczosSincFilter(2.0f, 2.0f, 3.0f), 0.2f, 0.2f, 0.712915421f)
        TestCaseData(LanczosSincFilter(1.0f, 1.0f, 3.0f), 1.0f, 1.0f, 0.0f)

        TestCaseData(TriangleFilter(1.0f, 1.0f), 0.0f, 0.0f, 1.0f)
        TestCaseData(TriangleFilter(1.0f, 1.0f), 0.5f, 0.0f, 0.5f)
        TestCaseData(TriangleFilter(1.0f, 1.0f), 0.5f, 0.5f, 0.25f)
    ]