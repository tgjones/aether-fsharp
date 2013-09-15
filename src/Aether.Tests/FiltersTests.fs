module Aether.Tests.FiltersTests

open NUnit.Framework
open Nexus
open Aether.Filters


[<TestCaseSource("testCases")>] 
let ``Filter Evaluate() works`` (filter : Filter) x y (expectedResult : single) =
    let result = filter.Evaluate x y
    Assert.That(result, Is.EqualTo(expectedResult))

let testCases =
    [ TestCaseData(BoxFilter(2.0f, 3.0f), 0.5f, 2.0f, 1.0f)
      TestCaseData(GaussianFilter(2.0f, 3.0f, 0.1f), 0.5f, 2.0f, 0.0804411918f)
      TestCaseData(MitchellFilter(2.0f, 3.0f, 0.3f, 0.7f), 0.5f, 2.0f, -0.0733335093f)
      TestCaseData(LanczosSincFilter(2.0f, 3.0f, 12.0f), 0.5f, 2.0f, 1.0f)
      TestCaseData(TriangleFilter(2.0f, 3.0f), 0.5f, 2.0f, 1.5f) ]