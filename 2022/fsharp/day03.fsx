let rucksacks =
    System.IO.File.ReadAllLines "2022/input/day03.txt"
    |> Array.map Seq.toList

let part1groups =
    rucksacks
    |> Array.map (fun rucksack ->
        rucksack
        |> List.splitInto 2
        |> List.map Set.ofList
    )

let part2groups =
    rucksacks
    |> Array.map Set.ofList
    |> Array.chunkBySize 3

let priority item =
    let itemVal = int item
    if itemVal > 96 then
        itemVal - 96
    else
        (itemVal + 26) - 64

let findPriorities groupsOfSets =
    groupsOfSets
    |> Array.map (fun items ->
        items
        |> Set.intersectMany
        |> Set.toList
        |> List.head
        |> priority
    )
    |> Array.sum

let part1 = findPriorities part1groups
let part2 = findPriorities part2groups

printfn $"{part1}"
printfn $"{part2}"
