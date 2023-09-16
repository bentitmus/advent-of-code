let sortedElves =
    System.IO.File.ReadAllText "2022/input/day01.txt"
    |> fun x -> x.TrimEnd()
    |> fun x -> x.Split "\n\n"
    |> Array.map (fun x -> x.Split "\n" |> Array.map int |> Array.sum)
    |> Array.sort
    |> Array.rev

let part1 = sortedElves[0]
let part2 =
    sortedElves
    |> Seq.take 3
    |> Seq.sum

printfn $"{part1}"
printfn $"{part2}"
