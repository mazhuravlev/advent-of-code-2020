module Day05

open System.IO

let makePairs s =
    [ for x in [ String.length s .. (-1) .. 1 ] do
        yield pown 2 x ]
    |> List.zip (s |> List.ofSeq)

let reduceFn up down acc (side, value) =
    let (|Up|_|) v = if v = up then Some Up else None
    let (|Down|_|) v = if v = down then Some Down else None

    match side with
    | Up -> acc + value / 2
    | Down -> acc
    | _ -> sprintf "invalid side: %c" side |> failwith

let processBspString up down part =
    makePairs part |> List.fold (reduceFn up down) 0

let bspToRowCol (bsp: string): int * int =
    let rowPart = bsp.Substring(0, 7)
    let colPart = bsp.Substring(7, 3)
    processBspString 'B' 'F' rowPart, processBspString 'R' 'L' colPart

let seatId (row, col) = row * 8 + col

let rec findMySeat (seatIds: int list) =
    match seatIds with
    | [] -> None
    | [ _ ] -> None
    | x :: xs ->
        match xs with
        | [] -> None
        | y ->
            let yh = y.Head

            match abs (yh - x) with
            | 2 -> (yh + x) / 2 |> Some
            | _ -> findMySeat xs

let run (Filename file) =
    let inputData = File.ReadAllLines file

    let sortedSeatIds =
        inputData
        |> Array.map (bspToRowCol >> seatId)
        |> Array.sort

    let result1 =
        sortedSeatIds
        |> Array.last
        |> sprintf "Max seat id is %A"

    let result2 =
        sortedSeatIds
        |> Array.toList
        |> findMySeat
        |> Option.map string
        |> Option.defaultValue "not found!"
        |> sprintf "My seat id is %s"

    (result1, result2)
