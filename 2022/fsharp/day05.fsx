#r @"nuget: FSharpPlus"
open FSharpPlus
open System.Text.RegularExpressions

let (unprocessedCrates, unprocessedMoves) =
    System.IO.File.ReadAllLines "2022/input/day05.txt"
    |> (flip Array.splitAt <*> Array.findIndex ((=) ""))

let unprocessedCratesWithoutIndices =
    Array.take (unprocessedCrates.Length - 1) unprocessedCrates

let crates =
    [| 1 .. 4 .. unprocessedCrates[0].Length |]
    |> Array.map (fun column -> (
        Array.map (String.item column) unprocessedCratesWithoutIndices
        |> Array.filter ((<>) ' ')
        |> Array.toList
    ))

let arrToTuple (arr: int array) = (arr[0], arr[1]-1, arr[2]-1)

let moves =
    unprocessedMoves[1..]
    |> Array.map (
        Regex("\D+").Split
        >> Array.filter ((<>) "")
        >> Array.map int
        >> arrToTuple
    )

let processMove mutation crates (numToMove, fromColumn, toColumn) =
    let origFromColumn = Array.item fromColumn crates
    let origToColumn = Array.item toColumn crates
    let items = List.take numToMove origFromColumn |> mutation
    let newFromColumn = List.skip numToMove origFromColumn
    let newToColumn = List.concat [ items; origToColumn ]
    Array.updateAt fromColumn newFromColumn crates
    |> Array.updateAt toColumn newToColumn

let processCrates mutation =
    Array.fold (processMove mutation) crates moves
    |> Array.map List.head
    |> System.String

let part1 = processCrates List.rev
let part2 = processCrates id

printfn $"{part1}"
printfn $"{part2}"
