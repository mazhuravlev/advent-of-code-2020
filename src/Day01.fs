module Day01

open System.IO

let run (Filename file) =
    let data = File.ReadAllLines file |> Array.map int

    let result1 =
        seq {
            for x in data do
                for y in data do
                    if x + y = 2020 then yield x * y
        }
        |> Seq.head
        |> sprintf "Product of two numbers that sum to 2020 is %i"

    let result2 =
        seq {
            for x in data do
                for y in data do
                    for z in data do
                        if x + y + z = 2020 then yield x * y * z
        }
        |> Seq.head
        |> sprintf "Product of three numbers that sum to 2020 is %i"

    (result1, result2)
