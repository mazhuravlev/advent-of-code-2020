[<AutoOpen>]
module Lib

type Result = string * string

type Filename = Filename of string

let splitStr (separator: string) (s: string) = s.Split(separator)

let splitSpaces = splitStr " "

let parseInt (s: string) =
    match System.Int32.TryParse(s) with
    | true, i -> Some i
    | _ -> None

let boolToInt =
    function
    | true -> 1
    | false -> 0
