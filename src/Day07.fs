module Day07

open System
open System.IO

let parseRule ruleStr =
    let concatSpaces = String.concat " "
    let words = splitSpaces ruleStr
    let bagName = concatSpaces words.[0..1]

    let innerBagReducer a (c: string) =
        let bagWords = splitSpaces c

        match parseInt bagWords.[0] with
        | Some count -> (concatSpaces bagWords.[1..2], count)
        | None ->
            sprintf "Bad int input: '%s' in string '%s'" bagWords.[0] c
            |> failwith
        :: a

    let innerBags =
        match ruleStr.EndsWith("no other bags.") with
        | true -> []
        | false ->
            (concatSpaces words.[4..])
            |> splitStr (", ")
            |> Array.fold innerBagReducer []

    (bagName, innerBags)

let bagCanBeContainedDirectly searchName (_, bagRule) =
    bagRule
    |> List.tryFind (fun (innerBagName, _) -> innerBagName = searchName)
    |> Option.isSome

let buildContainerBagList rules searchName =
    let rec buildContainerBagListRec searchName =
        let containerBags =
            rules
            |> List.filter (bagCanBeContainedDirectly searchName)

        let containerContainerBags =
            containerBags
            |> List.collect (fun (bagName, _) -> buildContainerBagListRec bagName)

        containerBags @ containerContainerBags

    buildContainerBagListRec searchName

let countContainedBags rules searchName' =
    let rec countContainedBagsRec searchName =
        let containedBags =
            rules
            |> List.tryFind (fun (bagName, _) -> bagName = searchName)
            |> Option.map snd

        match containedBags with
        | Some ruleList ->
            let ownCount = List.sumBy snd ruleList

            let innerCount =
                List.sumBy
                    (fun (bagName, bagCount) -> bagCount * (countContainedBagsRec bagName))
                    (ruleList
                     |> List.filter (fun (_, count) -> count > 0))

            innerCount + ownCount
        | None ->
            sprintf "Bag '%s' not found" searchName
            |> failwith

    countContainedBagsRec searchName'

let run (Filename file) =
    let rules =
        File.ReadAllLines file
        |> List.ofArray
        |> List.map parseRule

    let result1 =
        buildContainerBagList rules "shiny gold"
        |> List.map fst
        |> Set.ofList
        |> Set.count
        |> sprintf "Count of bags that can contain at least one 'shiny gold' is %i"

    let result2 =
        countContainedBags rules "shiny gold"
        |> sprintf "Count of bags that 'shiny gold' contains is %i"


    (result1, result2)
