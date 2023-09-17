let shiftChar ch b = (int ch) - (int b)

let score res shape = (res * 3) + (shape + 1)

let part1score (opp, shape) =
    let res = (shape + 4 - opp) % 3
    score res shape

let part2score (opp, res) =
    let shape = (opp + res + 2) % 3
    score res shape

let scores =
    System.IO.File.ReadAllLines "2022/input/day02.txt"
    |> Array.map (fun (line:string) ->
        line
        |> fun l -> (shiftChar l[0] 'A', shiftChar l[2] 'X')
        |> fun tup -> (part1score tup, part2score tup)
    )
    |> Array.fold (fun (xAcc, yAcc) (x, y) -> (xAcc + x, yAcc + y)) (0, 0)

let part1 = fst scores
let part2 = snd scores

printfn $"{part1}"
printfn $"{part2}"
