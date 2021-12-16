module Puzzel16b

open System.IO
open Advent2021.BITS
open FParsec
open Advent2021.Helpers

let solve () =
    let inputText = File.ReadAllText "16a.txt"    
    let binaryInput = run Parser.HexStringToBinaryStringParser inputText |> unwrapParserResult     
    let msg = run Parser.BitsParser binaryInput |> unwrapParserResult    
    
    let rec evaluate (msg: BitsMessage) : int64 =
        match msg.Packet with
            | Literal x -> x
            | Sum msgs ->
                msgs
                |> Array.map evaluate
                |> Array.sum
            | Product msgs ->
                msgs
                |> Array.map evaluate
                |> Array.fold
                    (fun state x -> x * state)
                    1
            | Minimum msgs  ->
                msgs
                |> Array.map evaluate
                |> Array.min
            | Maximum msgs  ->
                msgs
                |> Array.map evaluate
                |> Array.max
            | GT msgs ->
                let a = evaluate msgs[0]
                let b = evaluate msgs[1]
                if a > b then 1 else 0
            | LT msgs  ->
                let a = evaluate msgs[0]
                let b = evaluate msgs[1]
                if a < b then 1 else 0
            | EQ msgs ->
                let a = evaluate msgs[0]
                let b = evaluate msgs[1]
                if a = b then 1 else 0

    let result = evaluate msg
    
    printfn $"{result}"
    ()
    