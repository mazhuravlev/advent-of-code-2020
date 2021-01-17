module Day08

open System
open System.IO

type Operation =
    | Nop
    | Jmp
    | Acc

type Instruction = Instruction of Operation * int

let parseOperation s =
    match s with
    | "nop" -> Nop
    | "jmp" -> Jmp
    | "acc" -> Acc
    | _ -> sprintf "Invalid operation: %s" s |> failwith

let parseInstruction s =
    match splitSpaces s with
    | [| op; arg |] ->
        match parseInt arg with
        | Some i -> Instruction(parseOperation op, i)
        | None -> sprintf "Invalid argument: %s" s |> failwith
    | _ -> sprintf "Invalid instruction: %s" s |> failwith

let rec runProgram (program: Instruction array) acc ptr (visitedPtrs: Set<int>) =
    match Set.contains ptr visitedPtrs with
    | true -> acc
    | false ->
        let newVisitedPtrs = visitedPtrs |> Set.add ptr

        match program.[ptr] with
        | Instruction (Nop, _) -> runProgram program acc (ptr + 1) newVisitedPtrs
        | Instruction (Jmp, arg) -> runProgram program acc (ptr + arg) newVisitedPtrs
        | Instruction (Acc, arg) -> runProgram program (acc + arg) (ptr + 1) newVisitedPtrs

let run (Filename file) =
    let program =
        File.ReadAllLines file
        |> Array.map parseInstruction

    let result1 =
        runProgram program 0 0 Set.empty
        |> sprintf "Value of accumulator before beginning second loop is %i"

    (result1, "")
