namespace ``Sampling - Stratified sampler``
    open Xunit
    open FsUnit.Xunit
    open Aether.Sampling

    type ``Given a stratified sampler with a width and height of 2`` () =
        let sampler = StratifiedSampler(0, 2, 0, 2, 1, 1, false, 0.0f, 1.0f)

        [<Fact>] 
        let ``when GetNextSample() is called, it returns 4 samples``() =
            let rng = System.Random(1000)
            let samples = sampler.GetSamples rng |> Seq.toList

            samples.Length |> should equal 4

            samples.[0].ImageX |> should equal 0.5f
            samples.[0].ImageY |> should equal 0.5f

            samples.[1].ImageX |> should equal 1.5f
            samples.[1].ImageY |> should equal 0.5f

            samples.[2].ImageX |> should equal 0.5f
            samples.[2].ImageY |> should equal 1.5f

            samples.[3].ImageX |> should equal 1.5f
            samples.[3].ImageY |> should equal 1.5f