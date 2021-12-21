module Puzzel20a

open System.Collections.Immutable
open System.IO
open MathNet.Numerics.LinearAlgebra.Double
open Microsoft.FSharp.Core
open FParsec
open Advent2021.Helpers
open MathNet.Numerics.LinearAlgebra

module Parser =
    let lineParser = many (
                        choice [
                            pchar '.' 
                            pchar '#' 
                    ])

    let algoParser: Parser<char list, unit> =
        lineParser
        
    let imageParser =
        sepBy lineParser newline
        |>> array2D
    
    let inputParser = algoParser .>> newline .>> newline .>>. imageParser

let inputText = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"                

let hekjesToBin (input: char seq) =
    let binString = (seqToString input).Replace("#", "1").Replace(".", "0")
//    printfn $"{binString} {binaryStringToInt binString}"
    binaryStringToInt binString
        
let printImage image =
    let rowLength = Array2D.length1 image
    let columnLength = Array2D.length2 image
    for i in [0..rowLength - 1] do
        printfn ""
        for j in [0..columnLength - 1] do
            printf $"{image[i,j]}"
    printfn ""

let rec crop (image : char[,]) =
    let rowLength = Array2D.length1 image
    let columnLength = Array2D.length2 image
    if rowLength = 0 || columnLength = 0 // nothing to crop
    then image
    else if image[0,*] |> Array.forall ((=) '.') 
    then crop image[1..,*]  // crop first row
    else if image[*,0] |> Array.forall ((=) '.')
    then crop image[*,1..] // crop first column
    else if image[rowLength - 1,*] |> Array.forall ((=) '.')
    then crop image[0..rowLength - 2,*] // crop last row 
    else if image[*,columnLength - 1] |> Array.forall ((=) '.')
    then crop image[*, 0..columnLength - 2] // crop last column
    else image

let rec shrink (image : char[,]) =
    let rowLength = Array2D.length1 image
    let columnLength = Array2D.length2 image
    if rowLength = 0 || columnLength = 0 // nothing to crop
    then image
    else crop image[1..rowLength - 2,1..columnLength - 2]  // crop first row

let pad (image : char[,]) =
    let padded = Array2D.create ((Array2D.length1 image) + 2) ((Array2D.length2 image) + 2) '.'
    Array2D.blit image 0 0 padded 1 1 (Array2D.length1 image) (Array2D.length2 image)
    padded
    
let enhance (algo : char list) (image : char[,])=
    let result = Array2D.create (Array2D.length1 image) (Array2D.length2 image) '.'
    for row in [1..(Array2D.length1 image) - 2] do
        for column in [1..(Array2D.length2 image) - 2] do
            let row1 = image[row - 1, column - 1..column + 1] |> List.ofArray
            let row2 = image[row, column - 1..column + 1]  |> List.ofArray
            let row3 = image[row + 1, column - 1..column + 1] |> List.ofArray
            let index = hekjesToBin (row1 @ row2 @ row3)
            let newChar = algo[index]
//            printfn $"row:{row}, column:{column}, old:{paddedImage[row,column]}, new:{newChar}"
//            printfn $"new char = {newChar}"
            Array2D.set result row column newChar
    result
      
let solve () =
    let inputText = File.ReadAllText "20a.txt"
    let algo, image = run Parser.inputParser inputText |> unwrapParserResult
    printfn $"Algo heeft {List.length algo} items"
    printfn $"Image heeft {Array2D.length1 image} rijen en {Array2D.length2 image} kolommen"

    let e1 = image |> pad |> pad |> pad |> pad |> pad |> pad |> pad |> pad |> pad |> enhance algo |> shrink
    let e2 = e1 |> shrink |> enhance algo
    
    image |> printImage 
    e1 |> printImage 
    e2 |> printImage
    
    let count = seq {
                        for row in [0..(Array2D.length1 e2) - 1] do
                            for column in [0..(Array2D.length2 e2) - 1] do
                                if Array2D.get e2 row column = '#' then yield 1            
                    } |> Seq.sum
    printfn $"{count}"
    ()
    