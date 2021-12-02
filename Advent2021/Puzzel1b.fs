module Puzzel1b

open System.IO

let solve () = 
    let input = File.ReadAllLines "1a.txt"
    let result = 
        input
        |> Seq.map int
        |> Seq.windowed 3
        |> Seq.map (fun xs -> xs |> Seq.sum)
        |> Seq.windowed 2
        |> Seq.map (fun xs -> if xs.[1] > xs.[0] then 1 else 0)
        |> Seq.sum
    printfn $"{result}"