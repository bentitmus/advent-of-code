let rucksacks =
    System.IO.File.ReadAllLines "2022/input/day03.txt"
    |> Array.map Seq.toArray

let part1groups =
    rucksacks
    |> Array.map (
      Array.splitInto 2
      >> Array.map Set.ofArray
    )

let part2groups =
    rucksacks
    |> Array.map Set.ofArray
    |> Array.chunkBySize 3

let priority item =
    let itemVal = int item
    if itemVal > 96 then
        itemVal - 96
    else
        (itemVal + 26) - 64

let findPriorities =
    Array.map (
        Set.intersectMany
        >> Set.toList
        >> List.head
        >> priority
    )
    >> Array.sum

let part1 = findPriorities part1groups
let part2 = findPriorities part2groups

printfn $"{part1}"
printfn $"{part2}"
