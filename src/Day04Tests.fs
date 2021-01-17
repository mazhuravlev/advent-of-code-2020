module Day04Tests

open FsUnit.Xunit
open Xunit
open Day04

type Day04Tests() =

    [<Fact>]
    let ``can parse data line`` () =
        let result = parseDataLine "byr:1975 iyr:2011"
        let expected = [ ("byr", "1975"); ("iyr", "2011") ]
        Assert.Equal(expected, result)
