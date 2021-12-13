module Puzzel13b

open System
open System.IO
open FParsec
open Advent2021.Helpers

let inputText = "start-A
start-b
A-c
A-b
b-d
A-end
b-end"

type CaveType =
| Big of string
| Small of string
    
let solve () =
    let inputText = File.ReadAllText "12a.txt"
    
    let bigCaveParser = many1Chars asciiUpper |>> Big
    let smallCaveParser = many1Chars asciiLower |>> Small
    let caveParser = choice [bigCaveParser;smallCaveParser]
    let lineParser = sepEndBy caveParser (pchar '-')
    let inputParser = sepEndBy lineParser newline
    
    let input =
        run inputParser inputText |> unwrapParserResult
        |> List.map (fun parts -> (parts[0], parts[1]))
    
    let nodes =
        let ab =
            input
            |> List.groupBy fst
            |> List.map (fun (key, items) -> (key, List.map snd items))
        let ba =
            input
            |> List.groupBy snd
            |> List.map (fun (key, items) -> (key, List.map fst items))
        
        ab @ ba // omdat een cave in beide lijsten kan zitten moeten de bestemmingen van beide gecombineerd worden
        |> List.groupBy fst
        |> List.map (fun (key, lists) ->
            let destinations =
                lists
                |> List.map snd
                |> List.collect id                
            (key, destinations))
        |> Map.ofList            
            
    for node in nodes do
        printfn $"{node.Key} - {node.Value}"
       
    let startNode = Small "start"
    let endNode = Small "end"
    
    let rec findAllPaths (visitedNodes : CaveType list) (node: CaveType) =
//        let visitedString = String.Join(",", visitedNodes)
        
//        printfn $"findAllPaths {visitedString} {node}"
        
        let pathUntilNow = visitedNodes @ [node]
    
        if node = endNode
        then [pathUntilNow]
        else 
            let possibleNextNodes = Map.find node nodes
            possibleNextNodes
            |> List.collect (fun c ->
                match c with
                | Big _ -> findAllPaths pathUntilNow c
                | Small _ -> if List.contains c pathUntilNow then [pathUntilNow] else findAllPaths pathUntilNow c)
            
            
    
    let allPathsFromStart =
        findAllPaths [] startNode
        |> List.filter (fun p -> List.last p = endNode)
        |> List.length
    printfn $"{allPathsFromStart}"
    ()
    