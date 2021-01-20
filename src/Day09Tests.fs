module Day09Tests

open FsUnit.Xunit
open Xunit
open Day09

let inList = [ 1UL; 2UL; 3UL; 4UL; 5UL ]

type Day09Tests() =
    [<Fact>]
    let ``Can test if number is sum of numbers from list`` () =
        Assert.Equal(true, isSum inList 9UL)
        Assert.Equal(true, isSum inList 3UL)

    [<Fact>]
    let ``Can test if number is not sum of numbers from list`` () =
        Assert.Equal(false, isSum inList 20UL)
        Assert.Equal(false, isSum inList 1UL)

    [<Fact>]
    let ``Can test if array satisfies isSum`` () =
        Assert.Equal(true, isSumArr (inList @ [ 9UL ] |> Array.ofList))

        Assert.Equal(true, isSumArr (inList @ [ 3UL ] |> Array.ofList))

    [<Fact>]
    let ``Can test if array not satisfies isSum`` () =
        Assert.Equal(false, isSumArr (inList @ [ 20UL ] |> Array.ofList))

        Assert.Equal(false, isSumArr (inList @ [ 1UL ] |> Array.ofList))

    [<Fact>]
    let ``Can find sum numbers`` () =
        let actual = takeUntilSum 6UL inList []
        let expected = [ 3UL; 2UL; 1UL ]
        expected |> should equal actual

    [<Fact>]
    let ``Returns when sum numbers cant be found`` () =
        let actual = takeUntilSum 20UL inList []
        let expected = List.empty<uint64>
        expected |> should equal actual
