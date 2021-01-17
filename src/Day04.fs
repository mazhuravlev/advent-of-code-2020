module Day04

open System.IO
open System.Text.RegularExpressions

type PassportData =
    { Byr: string option
      Iyr: string option
      Eyr: string option
      Hgt: string option
      Hcl: string option
      Ecl: string option
      Pid: string option
      Cid: string option }

type Passport =
    | Passport of PassportData
    | IncompletePassport of PassportData

let emptyPassportData =
    { Byr = None
      Iyr = None
      Eyr = None
      Hgt = None
      Hcl = None
      Ecl = None
      Pid = None
      Cid = None }

type KvData = (string * string) array

let parseDataLine (line: string): KvData =
    let pairToTuple (pair: string) =
        let parts = pair.Split(":")

        match parts.Length with
        | 2 -> Some(parts.[0], parts.[1])
        | _ ->
            sprintf "illegal pair of length %d: %s" pair.Length pair
            |> failwith

    line |> splitSpaces |> Array.choose pairToTuple

let validateInt f v =
    v
    |> parseInt
    |> Option.map f
    |> Option.defaultValue false

let validateHeight h =
    let regexMatch = Regex.Match(h, @"^(\d+)(cm|in)$")
    let units = regexMatch.Groups.[2].Value

    match regexMatch.Success with
    | true ->
        parseInt regexMatch.Groups.[1].Value
        |> Option.map (
            (fun x ->
                match units with
                | "cm" -> x >= 150 && x <= 193
                | "in" -> x >= 59 && x <= 76
                | _ -> sprintf "invalid units: %s" units |> failwith)
        )
        |> Option.defaultValue false
    | false -> false

let applyKvDataToPassportData (kvData: KvData) (pd: PassportData) =
    let folder pd kv =
        match kv with
        | ("byr", v) -> { pd with Byr = Some v }
        | ("iyr", v) -> { pd with Iyr = Some v }
        | ("eyr", v) -> { pd with Eyr = Some v }
        | ("hgt", v) -> { pd with Hgt = Some v }
        | ("hcl", v) -> { pd with Hcl = Some v }
        | ("ecl", v) -> { pd with Ecl = Some v }
        | ("pid", v) -> { pd with Pid = Some v }
        | ("cid", v) -> { pd with Cid = Some v }
        | (k, v) -> sprintf "unknown k: %s; v: %A" k v |> failwith

    Array.fold folder pd kvData

let parsePassports lineList =
    let reducer acc (line: string) =
        let passportTypeMismatchErrorMsg =
            "Passport is invalid input at this point, must be an IncompletePassport"

        match line.Trim() with
        | "" ->
            match acc with
            | (IncompletePassport pd) :: xs ->
                IncompletePassport emptyPassportData
                :: ((Passport pd) :: xs)
            | (Passport (_)) :: _ -> failwith passportTypeMismatchErrorMsg
            | [] -> acc
        | dataLine ->
            let applyKvData =
                parseDataLine dataLine
                |> applyKvDataToPassportData

            match acc with
            | (IncompletePassport pd) :: xs -> (applyKvData pd |> IncompletePassport) :: xs
            | (Passport pd) :: xs -> failwith passportTypeMismatchErrorMsg
            | [] ->
                [ applyKvData emptyPassportData
                  |> IncompletePassport ]

    let result = Array.fold reducer [] lineList

    match result with
    | (IncompletePassport pd) :: xs -> (Passport pd) :: xs
    | _ -> result


let requiredFieldValuesAreVaild passport =
    match passport with
    | Passport pd ->
        match pd with
        | { Byr = Some byr
            Iyr = Some iyr
            Eyr = Some eyr
            Hgt = Some hgt
            Hcl = Some hcl
            Ecl = Some ecl
            Pid = Some pid
            Cid = _ } ->
            validateInt (fun x -> x >= 1920 && x <= 2002) byr
            && validateInt (fun x -> x >= 2010 && x <= 2020) iyr
            && validateInt (fun x -> x >= 2020 && x <= 2030) eyr
            && validateHeight hgt
            && (fun x -> Regex.IsMatch(x, @"^#[0-9a-f]{6}$")) hcl
            && (fun x -> Regex.IsMatch(x, @"^(amb|blu|brn|gry|grn|hzl|oth)$")) ecl
            && (fun x -> Regex.IsMatch(x, @"^\d{9}$")) pid
        | _ -> false
    | IncompletePassport _ -> failwith "IncompletePassport is invalid argument"

let requiredFieldsPresent passport =
    match passport with
    | Passport pd ->
        match pd with
        | { Byr = Some _
            Iyr = Some _
            Eyr = Some _
            Hgt = Some _
            Hcl = Some _
            Ecl = Some _
            Pid = Some _
            Cid = _ } -> true
        | _ -> false
    | IncompletePassport _ -> failwith "IncompletePassport is invalid argument"


let run (Filename file) =
    let passports = File.ReadAllLines file |> parsePassports

    let result1 =
        List.fold (fun a c -> a + (c |> requiredFieldsPresent |> boolToInt)) 0 passports
        |> sprintf "Number of passports with present fields is %i"

    let result2 =
        List.fold
            (fun a c ->
                a
                + (c |> requiredFieldValuesAreVaild |> boolToInt))
            0
            passports
        |> sprintf "Number of passports with valid fields is %i"

    (result1, result2)
