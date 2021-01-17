module Day07Tests

open FsUnit.Xunit
open Xunit
open Xunit.Abstractions
open Day07

type Day07Tests(output: ITestOutputHelper) =
    let write result =
        output.WriteLine(sprintf "The actual result was %A" result)

    [<Fact>]
    let ``can parse a rule with inner bags`` () =
        let result =
            parseRule "light red bags contain 1 bright white bag, 2 muted yellow bags."

        let expected =
            ("light red",
             [ ("muted yellow", 2)
               ("bright white", 1) ])

        Assert.Equal(expected, result)

    [<Fact>]
    let ``can parse a rule without inner bags`` () =
        let result =
            parseRule "dotted black bags contain no other bags."

        let expected = ("dotted black", [])
        Assert.Equal(expected, result)
