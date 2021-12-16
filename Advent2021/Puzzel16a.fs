module Puzzel16a

open System.IO
open Advent2021
open Advent2021.BITS
open FParsec
open Advent2021.Helpers

//let inputText = "D2FE28"
//let inputText = "EE00D40C823060"

let solve () =
    let inputText = File.ReadAllText "16a.txt"    
    let binaryInput = run Parser.HexStringToBinaryStringParser inputText |> unwrapParserResult     
    let msg = run Parser.BitsParser binaryInput |> unwrapParserResult

    let rec sumVersions (msg: BitsMessage) =
        seq {
            yield msg.Version
            match msg.Packet with
            | Literal _ -> ()
            | Sum msgs
            | Product msgs 
            | Minimum msgs 
            | Maximum msgs 
            | GT msgs 
            | LT msgs 
            | EQ msgs ->
                for m in msgs do
                    yield sumVersions m      
        }
        |> Seq.sum
    let result = sumVersions msg
    
    printfn $"{result}"
    ()
    