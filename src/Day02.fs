module Day02

open System.IO
open System.Text.RegularExpressions

let parse (s: string) =
    let m =
        Regex.Match(s, @"(\d+)-(\d+) (\w): (\w+)")

    let gv (n: int) = m.Groups.[n].Value

    match m.Success with
    | true -> ((gv 1 |> int), int (gv 2), (gv 3 |> char), gv 4)
    | false -> failwith "match error"

let isValidByLetterPositions (min, max, letter, password: string) =
    password.[min - 1] = letter
    <> (password.[max - 1] = letter)

let isValidByLetterCount (min, max, letter, password: string) =
    let letterCount =
        password
        |> List.ofSeq
        |> List.filter (fun c -> c = letter)
        |> List.length

    letterCount >= min && letterCount <= max

let run (Filename file) =
    let data =
        File.ReadAllLines file |> Array.map parse

    let result1 =
        data
        |> Array.sumBy (isValidByLetterCount >> boolToInt)
        |> sprintf "Number of valid passwords by count policy is %i"

    let result2 =
        data
        |> Array.sumBy (isValidByLetterPositions >> boolToInt)
        |> sprintf "Number of valid passwords by position policy is %i"

    (result1, result2)
