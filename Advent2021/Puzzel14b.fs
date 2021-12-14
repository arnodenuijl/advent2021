module Puzzel14b

open System
open System.IO
open System.Threading
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
//    let inputText = File.ReadAllText "14a.txt"
    let template, insertions = run Parser.inputParser inputText |> unwrapParserResult       
    printfn $"{seqToString template} {List.length insertions}"
    
    let replacements =
        insertions
        |> Map.ofList
    
    let AddOrUpdate key valueToAdd map =
        let v = Map.tryFind key map |> Option.defaultValue 0L
        Map.add key (v + valueToAdd) map
        
    let startState: Map<Char * Char,int64> =
        '-' :: template
        |> List.pairwise
        |> List.fold
                (fun (countPerChar: Map<Char * Char, int64>) (char: Char * Char) ->
                        countPerChar
                        |> AddOrUpdate char 1L)                
                Map.empty<Char * Char, int64>

    let takeStep (line: Map<Char * Char, int64>) =
        line
        |> Map.fold
            (fun state key value ->
                match Map.tryFind key replacements with
                | Some r ->
                    let newKey1 = (fst key, r)
                    let newKey2 = (r, snd key)
                    state
                    |> AddOrUpdate newKey1 value 
                    |> AddOrUpdate newKey2 value 
                | None -> state                
            )
            line 
    
    let getLetterCount (input: Map<Char * Char, int64>) =
        input
        |> Seq.map (fun kvp -> (snd kvp.Key, kvp.Value))
    
    printfn ""
    startState
    |> Map.iter (fun k v -> printfn $"{k} {v}")

    let endState =
        [0..9]
        |> List.fold
            (fun state _ ->
                let result = takeStep state
                printfn ""
                result
                |> Map.iter (fun k v -> printfn $"{k} {v}")
                result
                )
                
            startState            
        |> getLetterCount

    let min =
        endState
        |> Seq.minBy snd
        |> snd
        
    let max =
        endState
        |> Seq.maxBy snd
        |> snd
                       
    let result = max - min
        
    printfn $"{max} - {min} = {result}"
    ()

    // 2 = 1
    // 3 = 2
    // 4 = 3
    // 5 = 4
    // 6 = 5
    //
    // 123456
    
    // NBCCNBBBCBHCB                      -> 13 
    // NB BC CC CN NB BB BB BC CB BH HC CB = 12
    