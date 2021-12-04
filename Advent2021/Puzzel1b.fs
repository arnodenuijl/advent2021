module Puzzel1b

open System.IO

let solve () = 
    let input = File.ReadAllLines "1a.txt"
    let result = 
        input
        |> Seq.map int
        |> Seq.windowed 3
        |> Seq.map (fun xs -> xs |> Seq.sum)
        |> Seq.pairwise
        |> Seq.filter (fun (a, b) -> a > b)
        |> Seq.length
    printfn $"{result}"
