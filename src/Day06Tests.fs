module Day06Tests

open FsUnit.Xunit
open Xunit
open Xunit.Abstractions
open Day06

type Day07Tests() =
    [<Fact>]
    let ``makes no groups from empty input`` () =
        let result = makeGroups []
        result |> List.isEmpty |> should equal true

    [<Fact>]
    let ``makes 1 group from continuous input`` () =
        let result =
            "abc"
            |> List.ofSeq
            |> List.map string
            |> makeGroups

        result |> should equal [ [ "c"; "b"; "a" ] ]

    [<Fact>]
    let ``makes multiple groups from discontinuous input`` () =
        let result =
            "abc\ndef"
            |> List.ofSeq
            |> List.map string
            |> makeGroups

        result
        |> should equal [ [ "f"; "e"; "d" ]; [ "c"; "b"; "a" ] ]

    [<Fact>]
    let ``vote count for 1 group with 1 vote should be 1`` () =
        let result = [ "a" ] |> countUnanimousVotes
        result |> should equal 1

    [<Fact>]
    let ``vote count for multiple groups with same votes should be vote count`` () =
        let result =
            [ "ab"; "ab"; "ab" ] |> countUnanimousVotes

        result |> should equal 2

    [<Fact>]
    let ``vote count for group with different votes should be 0`` () =
        let result =
            [ "ab"; "cd"; "ef" ] |> countUnanimousVotes

        result |> should equal 0
