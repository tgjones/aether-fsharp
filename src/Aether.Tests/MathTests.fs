namespace Math

open Xunit
open Xunit.Extensions
open FsUnit.Xunit
open Aether.Math


type ``Global functions`` () =

    [<Fact>]
    let ``arithmetic constants should be correct`` () =
        pi        |> should (equalWithin 0.01f) 3.14f
        invPi     |> should (equalWithin 0.01f) 0.32f
        invTwoPi  |> should (equalWithin 0.01f) 0.16f
        invFourPi |> should (equalWithin 0.01f) 0.08f
        invLog2   |> should (equalWithin 0.01f) 1.44f

    [<Theory>]
    [<InlineData( 0.3f, 1.6f)>]
    [<InlineData( 0.0f, 1.0f)>]
    [<InlineData( 1.0f, 3.0f)>]
    let ``lerp linearly interpolates between two values`` value expectedValue =
        lerp value 1.0f 3.0f |> should equal expectedValue

    [<Theory>]
    [<InlineData(-0.5f, 0.0f)>]
    [<InlineData( 0.0f, 0.0f)>]
    [<InlineData( 0.5f, 0.5f)>]
    [<InlineData( 1.0f, 1.0f)>]
    [<InlineData( 1.5f, 1.0f)>]
    let ``clamp restricts a value to lie between a lower and higher bound`` value expectedValue =
        clamp value 0.0f 1.0f |> should equal expectedValue

    [<Fact>]
    let ``toRadians converts an angle in degrees to an angle in radians`` () =
        toRadians 90.0f |> should equal (pi / 2.0f)

    [<Fact>]
    let ``toDegrees converts an angle in radians to an angle in degrees`` () =
        toDegrees pi |> should equal 180.0f

    [<Theory>]
    [<InlineData(1.0f, 0.0f)>]
    [<InlineData(2.0f, 1.0f)>]
    [<InlineData(4.0f, 2.0f)>]
    [<InlineData(8.0f, 3.0f)>]
    [<InlineData(16.0f, 4.0f)>]
    [<InlineData(32.0f, 5.0f)>]
    let ``log2 calculates the binary logarithm`` value expectedValue =
        log2 value |> should equal expectedValue

    [<Fact>]
    let ``floor2int calculates the floor of a number, cast to an integer`` () =
        floor2int 3.4f |> should equal 3

    [<Fact>]
    let ``log2int calculates the binary logarithm of a number, cast to an integer`` () =
        log2int 2.5f |> should equal 1

    [<Theory>]
    [<InlineData(0, true)>]
    [<InlineData(1, true)>]
    [<InlineData(2, true)>]
    [<InlineData(3, false)>]
    [<InlineData(4, true)>]
    [<InlineData(6, false)>]
    [<InlineData(8, true)>]
    let ``isPowerOf2 returns true for numbers that are powers of two`` value expectedValue =
        isPowerOf2 value |> should equal expectedValue

    [<Fact>]
    let ``swap swaps the left and right values`` () =
        let mutable a = 1
        let mutable b = 2
        swap &a &b
        a |> should equal 2
        b |> should equal 1

    [<Fact>]
    let ``quadratic solves quadratic equations`` () =
        quadratic 1.0f 2.0f 3.0f |> should equal (Option<single>.None, Option<single>.None)
        quadratic 1.0f 5.0f 6.0f |> should equal (Some(-3.0f), Some(-2.0f))