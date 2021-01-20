module Day09

let isSum numbers n =
    seq {
        for x in numbers do
            for y in numbers do
                if x <> y && x + y = n then yield true
    }
    |> Seq.contains true

let isSumArr (nArr: uint64 array) =
    let numbers = nArr.[0..nArr.Length - 2]
    let n = nArr.[nArr.Length - 1]
    isSum numbers n

let rec takeUntilSum s leftNs ns =
    if s = 0UL then
        ns
    else if s < 0UL then
        []
    else
        match leftNs with
        | [] -> []
        | x :: xs -> takeUntilSum (s - x) xs (x :: ns)

let findContigousSums numbers n =
    let rec fn numbers sums =
        match numbers with
        | x :: xs -> (takeUntilSum n xs []) :: (fn xs sums)
        | _ -> []

    fn numbers (takeUntilSum n numbers [])
    |> List.filter (fun x -> x.Length > 0)

let findInvalidNumber =
    Seq.windowed 26
    >> (Seq.find (isSumArr >> not))
    >> Array.last

let run (Filename file) =
    let data =
        System.IO.File.ReadAllLines file
        |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
        |> Seq.map uint64

    let getResult1 =
        findInvalidNumber
        >> sprintf "First number which is not sum of previous 25 numbers is %i"

    let getResult2 =
        findInvalidNumber 
        >> findContigousSums (List.ofSeq data)
        >> List.min
        >> List.sort
        >> (fun l -> l.Head + (l |> List.rev |> List.head))
        >> sprintf "Sum of min and max of contigous sequence that sums to invalid number is %i"

    getResult1 data, getResult2 data
