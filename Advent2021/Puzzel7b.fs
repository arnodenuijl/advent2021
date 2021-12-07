module Puzzel7b

open FParsec
open Advent2021.Helpers
open System
open System.IO
open FSharp.Collections.ParallelSeq

let inputText = "16,1,2,0,4,2,7,1,2,14"
let solve () =
    let inputText = File.ReadAllText "7a.txt"
    let inputParser = (sepBy pint32 (pchar ','))
    let initialState = run inputParser inputText |> unwrapParserResult |> Array.ofList
    let max = Seq.max initialState
    let min = Seq.min initialState
    printfn $"{Seq.length initialState} items. Min: {min}, Max: {max}"
    
    let calculateFactorial =
        let rec facImpl total number =
            if number = 0 then total
            else facImpl (total + number ) (number - 1)
    
        let mutable cache = Map.empty<int32, int32>
        fun n ->
            match Map.tryFind n cache with
            | Some x -> x
            | None ->
                let result = facImpl 0 n
                cache <- Map.add n result cache
                result
        
    let calculateCosts (input: int32 array) (target: int32) =
        let cost =
            input
            |> Array.map (fun i -> Math.Abs(target - i) |> calculateFactorial)
            |> Array.sum
        cost        
        
    let all =
        [min..max]
        |> Seq.map (fun x -> x, (calculateCosts initialState x))
    
    let winner =
        all
        |> Seq.minBy snd
    printfn  $"{winner}"