let parseArgs (argv: string []) =
    match argv.Length > 0 with
    | true ->
        match System.Int32.TryParse(argv.[0]) with
        | true, i -> Some i
        | _ -> None
    | false -> None

let days =
    [ (1, Day01.run)
      (2, Day02.run)
      (3, Day03.run)
      (4, Day04.run)
      (5, Day05.run)
      (6, Day06.run)
      (7, Day07.run) ]

[<EntryPoint>]
let main argv =
    let printResult (result1, result2) =
        printfn "Part 1. %s\nPart 2. %s" result1 result2

    let inputFilename dayN =
        sprintf "input/input%i.txt" dayN |> Filename

    let displayDayMessage =
        printfn "----------------\n|\tRunning Day %i\n----------------"

    match parseArgs argv with
    | Some day ->
        displayDayMessage day

        match List.tryFind (fun (n, _) -> n = day) days with
        | Some (_, dayFn) -> inputFilename day |> dayFn |> printResult
        | None -> sprintf "Inexistent day %i" day |> failwith
    | None ->
        days
        |> List.iter
            (fun (day, dayFn) ->
                displayDayMessage day
                inputFilename day |> dayFn |> printResult)

    0
