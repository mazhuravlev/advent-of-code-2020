module Day10

let run (Filename file) =
    let data =
        System.IO.File.ReadAllLines file
        |> List.ofArray
        |> List.map int
        |> List.sort

    let result1 =
        0 :: data
        |> List.pairwise
        |> List.map (fun (a, b) -> b - a)
        |> List.countBy id
        |> Map
        |> (fun x -> x.[1] * (x.[3] + 1))
        |> sprintf "Number of 1-jolt differences multiplied by the number of 3-jolt differences is %i"

    (result1, "")
