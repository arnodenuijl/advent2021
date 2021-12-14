module Puzzel14a

open System
open System.IO
open FParsec
open Advent2021.Helpers

let inputText = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

module Parser =
    let templateParser: Parser<char list,unit> = many asciiLetter .>> newline
    let insertionParser: Parser<(char * char) * char,unit> =
        asciiLetter .>>. asciiLetter .>> skipString " -> " .>>. asciiLetter
       
    let insertionsParser: Parser<((char * char) * char) list,unit> = sepBy insertionParser newline
    let inputParser = templateParser .>> newline .>>. insertionsParser


let solve () =
    let inputText = File.ReadAllText "14a.txt"
    let template, insertions = run Parser.inputParser inputText |> unwrapParserResult       
    printfn $"{seqToString template} {List.length insertions}"
    
    let replacements =
        insertions
        |> Map.ofList
    for r in replacements do
        printfn $"{r.Key} - {r.Value}"
        
    let takeStep line = 
        List.foldBack
                    (fun character state ->
                        match state with
                        | [] -> [character]
                        | head :: _ ->
                            match Map.tryFind (character, head) replacements with
                            | Some r -> character :: r :: state
                            | None -> character :: state 
                        )
                    line
                    []
    
    let endState =
        [0..9]
        |> List.fold
            (fun state _ -> takeStep state)
            template
            
    let min =
        endState
        |> List.countBy id
        |> List.minBy snd
        |> snd
        
    let max =
        endState
        |> List.countBy id
        |> List.maxBy snd
        |> snd
        
        
        
    printfn $"{max - min}"
    ()
    