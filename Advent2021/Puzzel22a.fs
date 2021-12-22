module Puzzel22a

open System
open System.Text
open System.Threading
open System.Timers
open Microsoft.FSharp.Core
open Advent2021.Helpers
open FParsec
let inputText = "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"

type InputRanges = {
    OnOrOf : Boolean
    XRange: int * int
    YRange: int * int
    ZRange: int * int    
}

module Parser =
    let xyzRangeParser = pint32 .>> skipString ".." .>>. pint32
    let xParser = skipString " x=" >>. xyzRangeParser
    let yParser = skipString ",y=" >>. xyzRangeParser
    let zParser = skipString ",z=" >>. xyzRangeParser
    let onOrOfParser = choice [
        pstring "on" |>> fun _ -> true
        pstring "off" |>> fun _ -> false
    ]
    
    let inputParser : Parser<InputRanges list, unit>=
        sepBy (pipe4 onOrOfParser xParser yParser zParser (fun o x y z -> {OnOrOf = o;XRange=x;YRange=y;ZRange=z})) newline
        
let solve () =
//    let input = run Parser.inputParser inputText |> unwrapParserResult
    let input = runParserOnFile Parser.inputParser () "22a.txt" UTF8Encoding.UTF8  |> unwrapParserResult
                |> List.take 20
    printfn $"{input.Length}"
    for i in input do
        printfn $"{i}"
    
    let addToWorld world input =
        let points =
            seq {
                for x in [fst input.XRange..snd input.XRange] do
                    for y in [fst input.YRange..snd input.YRange] do
                        for z in [fst input.ZRange..snd input.ZRange] do
                            yield (x,y,z)
            } |> Set.ofSeq
        printfn $"Set {points.Count} points to {input.OnOrOf}"
        if input.OnOrOf
        then Set.union world points
        else Set.difference world points
    
    let result =
        input
        |> List.fold addToWorld Set.empty
    printfn $"{result.Count}"
    ()    
     