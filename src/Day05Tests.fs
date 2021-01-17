module Day05Tests

open FsUnit.Xunit
open Xunit
open Day05

type Day05Tests() =

    [<Fact>]
    let ``make pairs for empty string`` () =
        let result = makePairs ""
        result |> List.isEmpty |> should equal true

    [<Fact>]
    let ``make pairs for a string`` () =
        let result = makePairs "FFFF"

        result
        |> should
            equal
            [ ('F', 16)
              ('F', 8)
              ('F', 4)
              ('F', 2) ]

    [<Fact>]
    let ``processBspString all downs should be 0`` () =
        let result = processBspString 'B' 'F' "FFFFFFFF"
        result |> should equal 0

    [<Fact>]
    let ``processBspString all ups should be 2^length-1`` () =
        let result = processBspString 'B' 'F' "BBBBBBB"
        result |> should equal 127

    [<Fact>]
    let ``bspRowToCol all downs should be 0`` () =
        let result = bspToRowCol "FFFFFFFLLL"
        result |> should equal (0, 0)

    [<Fact>]
    let ``bspRowToCol for const string`` () =
        let result = bspToRowCol "FFBFBBFLRL"
        result |> should equal (22, 2)

    [<Fact>]
    let ``finds my seat`` () =
        let seats = [ 1; 2; 3; 4 (*5;*) ; 6; 7; 8; 9 ]
        findMySeat seats |> should equal <| Some 5
