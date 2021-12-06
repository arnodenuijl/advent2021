module Puzzel5a

open System
open System.IO
open FParsec
open Advent2021.Helpers

type Point = int32 * int32
type LineSegment = Point * Point

let pointsInLineSegment (segment : LineSegment) =
     let (x1,y1),(x2,y2) = segment
     let startX = Math.Min (x1,x2)
     let eindX = Math.Max (x1,x2)
     let startY = Math.Min (y1,y2)
     let eindY = Math.Max (y1,y2)
     seq {
         for i in [startX..eindX] do
            for j in [startY..eindY] do
                yield (i,j)
     }

module Parsing =
    let CoordinateParser =
        pint32 .>> (pchar ',') .>>. pint32
        
    let LineSegmentParser : Parser<LineSegment, unit> =
         CoordinateParser .>> skipString " -> " .>>. CoordinateParser 
    
    let LineSegmentsParser : Parser<LineSegment list, unit> =
        sepEndBy1 LineSegmentParser newline
      
let testInput = @"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

let solve () =
    let fileContent = File.ReadAllText "5a.txt"
//    let fileContent = testInput
    let lines  = run Parsing.LineSegmentsParser fileContent |> unwrapParserResult
    printfn $"{Seq.length lines} lines"
    
    let filteredLines =
        lines
        |> Seq.filter (fun ((x1,y1),(x2,y2)) -> x1 = x2 || y1 = y2)

    filteredLines
    |> Seq.iter (fun x -> printfn $"{x}")

    let allPoints =
        filteredLines
        |> Seq.collect pointsInLineSegment
        |> Seq.sort

    allPoints
    |> Seq.iter (fun x -> printfn $"{x}")
        
    let result =
        allPoints
        |> Seq.countBy id
        |> Seq.filter (fun (key, count) -> count >= 2)
        |> Seq.length
    printfn $"{result} result"
    
    ()