module Puzzel7a

open FParsec
open Advent2021.Helpers
open System
open System.IO
open FSharp.Collections.ParallelSeq

let inputText = "16,1,2,0,4,2,7,1,2,14"
let solve () =
//    let inputText = File.ReadAllText "7a.txt"
    let inputParser = (sepBy pint32 (pchar ','))
    let initialState = run inputParser inputText |> unwrapParserResult |> Array.ofList
    let max = Seq.max initialState
    let min = Seq.min initialState
    printfn $"{Seq.length initialState} items. Min: {min}, Max: {max}"
    
    let calculateCosts (input: int32 array) (target: int32) =
        let cost =
            input
            |> Array.map (fun i ->  Math.Abs(target - i))
            |> Array.sum
        cost
        
        
    let all =
        [min..max]
        |> Seq.map (fun x -> x, (calculateCosts initialState x))
    
    let winner =
        all
        |> Seq.minBy snd
    printfn  $"{winner}"