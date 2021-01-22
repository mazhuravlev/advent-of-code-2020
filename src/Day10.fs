module Day10

let rec findNextValidAdapters n adapters =
    match adapters with
    | x :: xs ->
        match x - n > 3 with
        | true -> []
        | false -> x :: (findNextValidAdapters n xs)
    | [] -> []

let rec trimListUptoElement n aList =
    match aList with
    | x :: xs ->
        match x = n with
        | true -> aList
        | false -> (trimListUptoElement n xs)
    | [] -> []

let rec findAdapterCombinationsRec (counter: uint64 ref) adapters =
    match adapters with
    | x :: xs ->
        match findNextValidAdapters x xs with
        | [] -> counter := !counter + 1UL
        | nextAdapters ->
            nextAdapters
            |> List.iter (fun x -> findAdapterCombinationsRec counter (trimListUptoElement x adapters))
    | [] -> counter := !counter + 1UL

let findAdapterCombinations adapters =
    let counter = ref 0UL
    findAdapterCombinationsRec counter (0 :: adapters)
    !counter

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

    let result2 =
        data
        |> findAdapterCombinations
        |> sprintf "Total number of distinct ways you can arrange the adapters is %i"

    (result1, result2)
