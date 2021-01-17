module Day07

open System
open System.IO

let parseRule ruleStr =
    let concatSpaces = String.concat " "
    let words = splitSpaces ruleStr
    let bagName = concatSpaces words.[0..1]

    let innerBagReducer a (c: string) =
        let bagWords = splitSpaces c

        match parseInt bagWords.[0] with
        | Some count -> (concatSpaces bagWords.[1..2], count)
        | None ->
            sprintf "Bad int input: '%s' in string '%s'" bagWords.[0] c
            |> failwith
        :: a

    let innerBags =
        match ruleStr.EndsWith("no other bags.") with
        | true -> []
        | false ->
            (concatSpaces words.[4..])
            |> splitStr (", ")
            |> Array.fold innerBagReducer []

    (bagName, innerBags)

let run (Filename file) =
    let rules =
        File.ReadAllLines file |> Array.map parseRule

    ("doesn't", "work")
