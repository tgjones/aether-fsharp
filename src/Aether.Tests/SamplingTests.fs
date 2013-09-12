module Aether.Tests.SamplingTests

open NUnit.Framework
open Nexus
open Aether.Sampling


module public RegularSamplerTests =

    [<Test>] 
    let ``GetNextSample() returns correct samples``() =
        // Arrange.
        let sampler = RegularSampler(IntPoint2D(0, 0), IntPoint2D(2, 2))
    
        // Act.
        let samples = sampler.GetSamples() |> Seq.toList

        // Assert.
        Assert.That(samples, Has.Length.EqualTo(4))

        Assert.That(samples.[0].ImageX, Is.EqualTo(0))
        Assert.That(samples.[0].ImageY, Is.EqualTo(0))

        Assert.That(samples.[1].ImageX, Is.EqualTo(1))
        Assert.That(samples.[1].ImageY, Is.EqualTo(0))

        Assert.That(samples.[2].ImageX, Is.EqualTo(0))
        Assert.That(samples.[2].ImageY, Is.EqualTo(1))

        Assert.That(samples.[3].ImageX, Is.EqualTo(1))
        Assert.That(samples.[3].ImageY, Is.EqualTo(1))