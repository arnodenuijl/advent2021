module Puzzel6a

open FParsec
open Advent2021.Helpers
open System
open System.IO

let testInput = "3,4,3,1,2"
let solve () =
    let input = File.ReadAllText "6a.txt"
    let inputParser = (sepBy pint32 (pchar ','))
    let initialState = run inputParser input |> unwrapParserResult
    
    let tick (input : int32 list) : int32 list =
        List.foldBack
            (fun i (currentSquids, newSquids) ->
                match i with
                | 0 -> (6 :: currentSquids,8 :: newSquids)
                | _ -> i - 1 :: currentSquids, newSquids)            
            input
            ([],[])
        |> fun (existing, newSquids) -> existing @ newSquids
    let result = 
        [0..79]
        |> Seq.fold
            (fun state _ -> tick state)        
            initialState
        |> Seq.length
        
    printfn  $"{result}"