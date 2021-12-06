module Puzzel6b

open FParsec
open Advent2021.Helpers
open System.IO

let solve () =
    let input = File.ReadAllText "6a.txt"
    let inputParser = (sepBy pint32 (pchar ','))
    let initialState = run inputParser input |> unwrapParserResult    
    
    let groupedInitialState =
        [0..8]
        |> List.map (fun i ->
            i, initialState |> List.filter (fun l -> l = i) |> List.length |> int64)
        |> Map.ofList
        
    let tick (input : Map<int32, int64>) : Map<int32, int64> =
        [0..8]
        |> List.fold
               (fun newState i ->
                    match i with
                    | 8 ->
                        newState
                        |> Map.add 8 (Map.find 0 input)
                    | 6 ->
                        let new6 = (Map.find 0 input) + (Map.find 7 input)
                        newState
                        |> Map.add 6 new6
                    | _ ->
                        newState
                        |> Map.add i (Map.find (i + 1) input)                    
                    )
                input
                
    let result =
        [0..255]
        |> List.fold
            (fun state _ -> tick state)
            groupedInitialState
            
    let totalCount =
        result
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.sum            
    printfn $"{totalCount}" //1609058859115
    ()