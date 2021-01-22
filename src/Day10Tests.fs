module Day10Tests

open FsUnit.Xunit
open Xunit
open Day10

type Day10Tests() =

    [<Fact>]
    let ``Can find next valid adapters`` () =
        findNextValidAdapters 0 [ 1; 2; 3; 4; 5 ]
        |> should equal [ 1; 2; 3 ]

        findNextValidAdapters 0 [ 2; 3; 4; 5 ]
        |> should equal [ 2; 3 ]

        findNextValidAdapters 0 [ 3; 4; 5 ]
        |> should equal [ 3 ]

        findNextValidAdapters 0 [ 4; 5 ]
        |> should be Empty

    [<Fact>]
    let ``Can trim list up to given element`` () =
        let l = [ 1; 2; 3; 4; 5 ]

        trimListUptoElement 3 l
        |> should equal [ 3; 4; 5 ]

        trimListUptoElement 1 l
        |> should equal [ 1; 2; 3; 4; 5 ]

        trimListUptoElement 5 l |> should equal [ 5 ]

    [<Fact>]
    let ``Can find adapter combination count`` () =
        let input =
            [ 1
              4
              5
              6
              7
              10
              11
              12
              15
              16
              19 ]

        findAdapterCombinations input |> should equal 8UL

    [<Fact>]
    let ``Can find adapter combination count for long list`` () =
        let input =
            [ 1
              2
              3
              4
              7
              8
              9
              10
              11
              14
              17
              18
              19
              20
              23
              24
              25
              28
              31
              32
              33
              34
              35
              38
              39
              42
              45
              46
              47
              48
              49 ]

        findAdapterCombinations input
        |> should equal 19208UL
