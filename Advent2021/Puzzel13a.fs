module Puzzel13a

open System
open System.IO
open FParsec
open Advent2021.Helpers

let inputText = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"

type Fold =
| X of int32
| Y of int32


let solve () =
//    let inputText = File.ReadAllText "13a.txt"
    
    let dotParser = pint32 .>> (pchar ',') .>>. pint32
    let dotsParser = sepEndBy dotParser newline 
    let foldParser =
            choice [
                pstring "x=" >>. pint32 |>> X
                pstring "y=" >>. pint32 |>> Y
            ]
    let foldlineParser = pstring "fold along " >>. foldParser
    let foldlinesParser = sepBy foldlineParser newline
    
    let inputParser = dotsParser .>> newline .>>. foldlinesParser
    
    let dots, folds = run inputParser inputText |> unwrapParserResult
    printfn $"{List.length dots} dots and {List.length folds} folds"
    
    let maxX = dots |> Seq.map fst |> Seq.max
    let maxY = dots |> Seq.map snd |> Seq.max

    printfn $"{maxX} :: {maxY}"    
    let paper = Array2D.create (maxX + 1) (maxY + 1) false
    dots
    |> Seq.iter (fun (x, y) -> paper[x,y] <- true)
    
    
    let foldAlongX (foldPosY: int32) (paper: bool[,]) =
        let currentHeight = (Array2D.length2 paper)      
        let currentWidth = (Array2D.length1 paper)      
        let newHeight = Math.Max(foldPosY, currentHeight - foldPosY) 
        let newPaper = Array2D.create (Array2D.length1 paper) newHeight false
        printfn $"Created new paper with size {Array2D.length1 newPaper} {Array2D.length2 newPaper}"

        let newYPos y =
            let newY =
                if y < foldPosY
                then newHeight - (foldPosY - y)  // 7 - 8 + 1       8 - 9 + 1
                else newHeight - (y - foldPosY + 1)
            printfn $"Y: {y} -> {newY}"
            newY
            
        for x in [0..currentWidth - 1] do
            for y in [0..currentHeight - 1] do
                if y <> foldPosY then
                    let newY = newYPos y                    
                    let oldPaperValue = paper.[x,y]
                    let newPaperValue = paper.[x,newY] || oldPaperValue
                    printfn $"Set ({x},{newY}) to {newPaperValue}"
                    Array2D.set newPaper x newY newPaperValue
        newPaper
        
    let result =
        paper
        |> foldAlongX 1

    let boolToChar b = if b then "#" else "."
    
    
    let printPaper paper =
        for y in [0..(Array2D.length2 paper) - 1] do
            printfn ""
            for x in [0..(Array2D.length1 paper) - 1] do
                printf $"{boolToChar <| paper[x,y]}"
        printfn ""
    
    printPaper paper
    printPaper result
    

    ()
    