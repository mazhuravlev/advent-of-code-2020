module Day03

open System.IO

type Cell =
    | Empty
    | Tree

let parse =
    function
    | '.' -> Empty
    | '#' -> Tree
    | ic -> sprintf "invalid input char: %A" ic |> failwith

let slopeFolder mapWidth mapHeight (map: Cell [,]) (xSlope, ySlope) a c =
    let x = (c * xSlope) % mapWidth
    let y = (c * ySlope)

    match y < mapHeight with
    | true ->
        match map.[y, x] with
        | Empty -> a
        | Tree -> a + 1L
    | false -> a

let run (Filename file) =
    let map =
        File.ReadAllLines file
        |> Array.map (fun (line: string) -> line.ToCharArray() |> Array.map parse)
        |> array2D

    let mapHeight = map.GetLength 0
    let mapWidth = map.GetLength 1

    let ySeq =
        Seq.initInfinite id
        |> Seq.takeWhile (fun y -> y < mapHeight)

    let treeCountForSlope (x, y) =
        Seq.fold (slopeFolder mapWidth mapHeight map (x, y)) 0L ySeq

    let result1 =
        treeCountForSlope (3, 1)
        |> sprintf "Tree count on slope 'right 3, down 1' is %i"

    let result2 =
        [ (1, 1)
          (3, 1)
          (5, 1)
          (7, 1)
          (1, 2) ]
        |> List.fold (fun a c -> a * treeCountForSlope c) 1L
        |> sprintf "Product of tree counts on slopes is %i"

    (result1, result2)
