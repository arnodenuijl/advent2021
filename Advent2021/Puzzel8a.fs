module Puzzel8a

open FParsec
open Advent2021.Helpers
open System
open System.IO
open FSharp.Collections.ParallelSeq

let inputText = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
let solve () =
    let inputText = File.ReadAllText "8a.txt"
    let signalsParser = sepEndBy (many asciiLetter |>> Set.ofSeq) (pchar ' ')
    
    let signalAndOutputParser = signalsParser .>> skipString "| " .>>. signalsParser
    let inputParser = sepBy signalAndOutputParser skipNewline
    let patternsAndInputs = run inputParser inputText |> unwrapParserResult 
    
    let result =
        patternsAndInputs
        |> List.collect (fun (signals, outputs) -> outputs) // grab output
        |> List.filter (fun output -> let signalCount = Set.count output
                                      List.contains signalCount [2;3;4;7])
        |> List.length
    printfn  $"outputValues {result}"