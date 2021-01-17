module Day06

open System
open System.IO

let makeGroups aList =
    let rec makeGroupsRec groups buildingGroup remainingGroups =
        match remainingGroups with
        | [ x ] ->
            match String.IsNullOrWhiteSpace x with
            | true -> buildingGroup :: groups
            | false -> (x :: buildingGroup) :: groups
        | x :: xs ->
            match String.IsNullOrWhiteSpace x with
            | true -> makeGroupsRec (buildingGroup :: groups) [] xs
            | false -> makeGroupsRec groups (x :: buildingGroup) xs
        | [] -> groups

    makeGroupsRec [] [] aList

let countUnanimousVotes group =
    match group with
    | [ x ] -> String.length x
    | x :: xs ->
        List.fold (fun a c -> Set.intersect a (set c)) (set x) xs
        |> Set.count
    | _ -> 0

let run (Filename file) =
    let groups =
        File.ReadAllLines file
        |> List.ofArray
        |> makeGroups

    let result1 =
        groups
        |> List.fold (fun sum group -> sum + (String.concat "" group |> set |> Set.count)) 0
        |> sprintf "Vote count sum is %i"

    let result2 =
        groups
        |> List.fold (fun sum group -> sum + (countUnanimousVotes group)) 0
        |> sprintf "Unanimous vote count is %i"

    (result1, result2)
