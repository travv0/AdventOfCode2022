open System.IO

let parse (s: string) = s.Trim().Split('\n')

let findDup rucksack =
    let compartment1, compartment2 =
        Array.ofSeq rucksack
        |> Array.splitAt (Seq.length rucksack / 2)

    Set.intersect (Set.ofSeq compartment1) (Set.ofSeq compartment2)
    |> Set.toList
    |> List.head

let priority c =
    if c >= 'a' && c <= 'z' then
        int c - int 'a' + 1
    else
        int c - int 'A' + 27

let input = File.ReadAllText("input.txt")
let rucksacks = parse input

rucksacks
|> Seq.map (findDup >> priority)
|> Seq.sum
|> printfn "%A"

let findBadges rucksacks =
    rucksacks
    |> List.ofSeq
    |> List.chunkBySize 3
    |> List.map (fun [ rucksack1; rucksack2; rucksack3 ] ->
        Set.intersectMany [ Set.ofSeq rucksack1
                            Set.ofSeq rucksack2
                            Set.ofSeq rucksack3 ]
        |> List.ofSeq
        |> List.head)

findBadges rucksacks
|> Seq.map priority
|> Seq.sum
|> printfn "%A"
