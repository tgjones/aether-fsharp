namespace ``Sampling - Regular sampler``
    open Xunit
    open FsUnit.Xunit
    open Nexus
    open Aether.Sampling

    type ``Given a regular sampler with a width and height of 2`` () =
        let sampler = RegularSampler(IntPoint2D(0, 0), IntPoint2D(2, 2))

        [<Fact>] 
        let ``when GetNextSample() is called, it returns 4 samples``() =
            let samples = sampler.GetSamples() |> Seq.toList

            samples.Length |> should equal 4

            samples.[0].ImageX |> should equal 0
            samples.[0].ImageY |> should equal 0

            samples.[1].ImageX |> should equal 1
            samples.[1].ImageY |> should equal 0

            samples.[2].ImageX |> should equal 0
            samples.[2].ImageY |> should equal 1

            samples.[3].ImageX |> should equal 1
            samples.[3].ImageY |> should equal 1