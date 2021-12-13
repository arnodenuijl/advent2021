module Puzzel13b

open System
open System.IO
open FParsec
open Advent2021.Helpers

type Fold =
| X of int32
| Y of int32

module Parser =
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
    
let printPaper paper =
    let boolToChar b = if b then "#" else "."
    for y in [0..(Array2D.length2 paper) - 1] do
        printfn ""
        for x in [0..(Array2D.length1 paper) - 1] do
            printf $"{boolToChar <| paper[x,y]}"
    printfn ""

let solve () =
    let inputText = File.ReadAllText "13a.txt"
    let dots, folds = run Parser.inputParser inputText |> unwrapParserResult
    printfn $"{List.length dots} dots and {List.length folds} folds"
    
    let maxX, maxY = 
        dots
        |> Seq.fold
            (fun (maxX, maxY) (x,y) -> (Math.Max(maxX, x), Math.Max(maxY, y)))
            (0,0)
            
    printfn $"{maxX} :: {maxY}"    
    let paper = Array2D.create (maxX + 1) (maxY + 1) false
    dots
    |> Seq.iter (fun (x, y) -> paper[x,y] <- true)
    
    let foldPaper (fold : Fold) (paper: bool[,]) =
        let currentWidth = (Array2D.length1 paper)      
        let currentHeight = (Array2D.length2 paper)      
        let newWidth, newHeight =
            match fold with
            | X x -> Math.Max(x, currentWidth - x - 1), currentHeight
            | Y y -> currentWidth, Math.Max(y, currentHeight - y - 1)
        
        let newPos currentX currentY =
            let result =
                match fold with
                | Y y -> currentX, newHeight - Math.Abs(y - currentY)
                | X x -> newWidth - Math.Abs(x - currentX), currentY
            result

        let newPaper = Array2D.create newWidth newHeight false
        printfn $"Created new paper with size {newWidth} {newHeight}"
            
        for y in [0..currentHeight - 1] do
            for x in [0..currentWidth - 1] do
                match fold with
                | X foldX when x = foldX -> ()
                | Y foldY when y = foldY -> ()
                | _ ->
                    let newX, newY = newPos x y                    
                    let oldPaperValue = paper[x,y]
                    let newPaperValue = newPaper[newX,newY] || oldPaperValue
                    Array2D.set newPaper newX newY newPaperValue
        newPaper
        
    folds
    |> Seq.fold
           (fun state fold -> foldPaper fold state)
           paper
    |> printPaper
    
    