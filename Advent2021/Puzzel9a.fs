module Puzzel9a

open FParsec
open Advent2021.Helpers
open System
open System.IO
open FSharp.Collections.ParallelSeq

let inputText = "2199943210
3987894921
9856789892
8767896789
9899965678"

let solve () =
    let inputText = File.ReadAllText "9a.txt"
    let parser = sepEndBy (many (digit |>> charToInt)) newline
    
    let input = run parser inputText |> unwrapParserResult 
    let xMax = List.length input - 1 
    let yMax = List.length input[0] - 1 
        
    let getNeighbours i j =
        seq {
            if i > 0 then yield (i - 1,j) 
            if i < xMax then yield (i + 1,j)
            if j > 0 then yield (i,j - 1) 
            if j < yMax then yield (i,j + 1)            
        }
        |> Seq.map (fun (x, y) -> (x, y, input[x][y]))
   
    let mins =
        seq {
            for i in [0..xMax] do
                for j in [0..yMax] do
                    let current = input[i][j]
                    let _,_,lowestNeigbourHeight =
                        getNeighbours i j
                        |> Seq.minBy(fun (x,y,height) -> height)
                    if current < lowestNeigbourHeight
                    then yield  (i, j, current)                    
        }
    let result =
        mins
        |> Seq.map (fun (_,_,height) -> height + 1)
        |> Seq.sum
    
    printfn  $"outputValues {xMax}"
    printfn  $"outputValues {yMax}"
    printfn  $"outputValues {result}"