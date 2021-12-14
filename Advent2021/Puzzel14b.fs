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

let getLetterCount (input: Map<Char * Char, int64>) =
    input
    |> Seq.map (fun kvp -> (snd kvp.Key, kvp.Value))
    |> Seq.groupBy fst
    |> Seq.map (fun (k, vs) -> (k, vs |> Seq.map snd |> Seq.sum))

let solve () =
    let inputText = File.ReadAllText "14a.txt"

    let template, insertions = run Parser.inputParser inputText |> unwrapParserResult       
    printfn $"{seqToString template} & {List.length insertions} insertion"
    
    let replacements =
        insertions
        |> Map.ofList
    
    // de input paarsgewijs. Een - er voor om een paar te vormen met de eerste Char
    // Aan het eind tellen we alleen de tweede letter van de key (-,N) is dus een N en (B,N) is een N        
    let startState: Map<Char * Char,int64> =
        '-' :: template
        |> List.pairwise
        |> List.fold
                (fun (countPerChar: Map<Char * Char, int64>) (char: Char * Char) ->
                        countPerChar
                        |> SetOrUpdate char 1L ((+) 1L))                
                Map.empty<Char * Char, int64>
                
    let takeStep (line: Map<Char * Char, int64>) =
        line
        |> Seq.collect // collect all new pairs. Collect because we can return more than 1 output per input 
            (fun kvp ->
                match Map.tryFind kvp.Key replacements with
                | Some r -> // if we have a replacement we return the two new pairs. That have the same count as the original pair
                    let newKey1 = (fst kvp.Key, r)
                    let newKey2 = (r, snd kvp.Key)
                    [
                        (newKey1, kvp.Value) 
                        (newKey2, kvp.Value)
                    ]
                | None -> // if there is no replacement, we return the original 
                    [(kvp.Key,kvp.Value)]
            )
        |> Seq.fold
               (fun state (key, value) -> SetOrUpdate key value ((+) value) state)
               Map.empty<Char * Char, int64>
        
    let endState =
        [0..39]
        |> List.fold
            (fun state _ -> takeStep state)                
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