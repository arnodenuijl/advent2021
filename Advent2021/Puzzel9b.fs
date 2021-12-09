module Puzzel9b

open FParsec
open Advent2021.Helpers
open System.IO

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
        
    let rec getBassinPoints (knownPoints: Set<int*int*int>) i j : Set<int*int*int>=
        let currentValue = input[i][j]
        let neighbours = getNeighbours i j
                
        let higherNeighbours =
            neighbours
            |> Seq.filter (fun cs -> not <| Set.contains cs knownPoints) // filter out points we already know
            |> Seq.filter (fun (_,_, value) -> value >= currentValue && value < 9) 
            
        let initialState = Set.add (i,j, currentValue) knownPoints 
        higherNeighbours
        |> Seq.fold
                (fun state (neighbourI, neighbourJ, _) ->
                    getBassinPoints state neighbourI neighbourJ)
                initialState
                   
    let result =
        mins
        |> Seq.map (fun (i,j, _) -> getBassinPoints Set.empty i j)
        |> Seq.map Set.count
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.fold (*) 1 
    
    printfn  $"outputValues {xMax}"
    printfn  $"outputValues {yMax}"
    printfn  $"outputValues {result}"