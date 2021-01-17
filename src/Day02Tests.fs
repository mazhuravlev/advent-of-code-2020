module Day02Tests

open Xunit
open Day02

type Day02Tests() =

    [<Fact>]
    let ``boolToInt produces 0 for false and 1 for true`` () =
        Assert.Equal(1, boolToInt true)
        Assert.Equal(0, boolToInt false)

    [<Fact>]
    let ``can parse password policy`` () =
        let result = parse "1-3 a: abcde"
        Assert.Equal(result, (1, 3, 'a', "abcde"))

    [<Fact>]
    let ``validate by count for valid password should pass`` () =
        Assert.Equal(true, isValidByLetterCount (2, 4, 'a', "vaavvvv"))
        Assert.Equal(true, isValidByLetterCount (2, 4, 'a', "vaavvav"))
        Assert.Equal(true, isValidByLetterCount (2, 4, 'a', "vaavava"))

    [<Fact>]
    let ``validate by count for invalid password should fail`` () =
        Assert.Equal(false, isValidByLetterCount (2, 4, 'a', "vvvvvvv"))
        Assert.Equal(false, isValidByLetterCount (2, 4, 'a', "vaavaaa"))

    [<Fact>]
    let ``validate by positions for valid password should pass`` () =
        let result =
            isValidByLetterPositions (2, 4, 'a', "cabbc")

        Assert.Equal(true, result)

    [<Fact>]
    let ``validate by positions for invalid password should fail`` () =
        Assert.Equal(false, isValidByLetterPositions (2, 4, 'a', "vvvvvvv"))
        Assert.Equal(false, isValidByLetterPositions (2, 4, 'a', "aaaaaaa"))
