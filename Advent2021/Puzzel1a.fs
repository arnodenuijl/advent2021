module Puzzel1a

open System.IO

let solve () =
    let input = File.ReadAllLines "1a.txt"
    let result = 
        input
        |> Seq.map int
        |> Seq.windowed 2
        |> Seq.filter (fun xs -> xs.[1] > xs.[0])
        |> Seq.length
    printfn $"{result}"
    
