let splitRange (line:string) = line.Split([|','; '-'|])

let convertToIntTuple (line:string array) =
    (int(line[0]), int(line[1]), int(line[2]), int(line[3]))

let lowerFirst (begin1, end1, begin2, end2) =
    if begin1 > begin2 then
        (begin2, end2, begin1, end1)
    else
        (begin1, end1, begin2, end2)

let ranges =
    System.IO.File.ReadAllLines "2022/input/day04.txt"
    |> Array.map (splitRange >> convertToIntTuple >> lowerFirst)

let countMatching pred =
    ranges
    |> Seq.filter pred
    |> Seq.length

let containedWithin (begin1, end1, begin2, end2) =
    begin1 = begin2 || end1 >= end2

let overlaps (begin1, end1, begin2, end2) =
    end1 >= begin2

let part1 = countMatching containedWithin
let part2 = countMatching overlaps

printfn $"{part1}"
printfn $"{part2}"
