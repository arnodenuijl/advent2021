module Puzzel11a

open System
open System.IO
open FParsec
open Advent2021.Helpers

let inputText = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

let solve () =
    let inputText = File.ReadAllText "11a.txt"
    
    let parser = sepEndBy (many (digit |>> charToInt) |>> Array.ofList) newline
    let input = run parser inputText |> unwrapParserResult  |> Array.ofList
    
    let xMax = Array.length input - 1 
    let yMax = Array.length input[0] - 1 
    let mutable flashCounter = 0
    
    let getNeighbours x y =
        seq {
            if x > 0  && y > 0 then yield (x - 1, y - 1) 
            if x > 0 then yield (x - 1,y)                 
            if x > 0 && y < yMax then yield (x - 1, y + 1) 
            if y < yMax then yield (x,y + 1)
            if x < xMax && y < yMax then yield (x + 1, y + 1)
            if x < xMax then yield (x + 1, y)
            if x < xMax && y > 0 then yield (x + 1,y - 1)
            if y > 0 then yield (x,y - 1) 
        }
    
    let allPoints () = seq {
        for x in [0 .. xMax] do
            for y in [0 .. yMax] do
                yield (x,y)
    }
    
    let printInput () =
        for x in [0 .. xMax] do
            printfn ""
            for y in [0 .. yMax] do 
                printf $"{input.[x][y]}"
        printfn ""
    
    let step() =
            
        let rec flash x y =
            let neighbours = getNeighbours x y
            input[x][y] <- 0
            flashCounter <- flashCounter + 1
            for x,y in neighbours do
                let neighbourValue = input[x][y]                        
                if neighbourValue = 9 then
                    input[x][y] <- input[x][y] + 1
                    flash x y
                else if neighbourValue < 9 && neighbourValue > 0 then
                    input[x][y] <- input[x][y] + 1           
        
        // first
        allPoints()
        |> Seq.iter (fun (x,y) ->
            input.[x].[y] <- input.[x].[y] + 1)

        printInput()

        //second
        allPoints()
        |> Seq.iter (fun (x,y) -> if input[x][y] = 10 then flash x y)
        printInput()
        

    printInput()
    
    for _ in [0..99] do step()
    printfn $"{flashCounter}"
    ()
    