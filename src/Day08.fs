module Day08

type Operation =
    | Nop
    | Jmp
    | Acc

type Instruction = Instruction of Operation * int

type ExecutionResult =
    | Looped of int
    | Terminated of int

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

let runProgram (program: Instruction array) =
    let rec runProgram' acc ptr (visitedPtrs: Set<int>) =
        match ptr = program.Length - 1, Set.contains ptr visitedPtrs with
        | _, true -> Looped acc
        | true, false -> Terminated acc
        | false, false ->
            let newVisitedPtrs = visitedPtrs |> Set.add ptr

            match program.[ptr] with
            | Instruction (Nop, _) -> runProgram' acc (ptr + 1) newVisitedPtrs
            | Instruction (Jmp, arg) -> runProgram' acc (ptr + arg) newVisitedPtrs
            | Instruction (Acc, arg) -> runProgram' (acc + arg) (ptr + 1) newVisitedPtrs

    runProgram' 0 0 Set.empty

let permutateProgram (program: Instruction array) n =
    match program.[n] with
    | Instruction (Jmp, x) ->
        let program' = program.[0..]
        program'.[n] <- Instruction(Nop, x)
        Some program'
    | Instruction (Nop, x) ->
        let program' = program.[0..]
        program'.[n] <- Instruction(Jmp, x)
        Some program'
    | _ -> None


let run (Filename file) =
    let program =
        System.IO.File.ReadAllLines file
        |> Array.map parseInstruction

    let result1 =
        runProgram program
        |> sprintf "Value of accumulator before beginning second loop is %A"

    let result2 =
        [ 0 .. program.Length - 1 ]
        |> Seq.map (permutateProgram program)
        |> Seq.pick
            (fun prg ->
                match prg with
                | Some p ->
                    match runProgram p with
                    | Looped _ -> None
                    | Terminated x -> Some x
                | None -> None)
        |> sprintf "Accumumulator value after program termination %i"

    (result1, result2)
