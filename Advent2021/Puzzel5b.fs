module Puzzel5b

open System
open System.IO
open FParsec
open Advent2021.Helpers

type Point = int32 * int32
type LineSegment = Point * Point

let pointsInLineSegment (segment : LineSegment) =
    let (x1,y1),(x2,y2) = segment 
    let vectorX =
        if x1 < x2 then 1
        else if x1 = x2 then 0
        else -1
        
    let vectorY =
        if y1 < y2  then 1
        else if y1 = y2 then 0
        else -1
    
    seq {
        let mutable current = (x1,y1)
        while current <> (x2,y2) do
            yield current
            current <- (fst current + vectorX, snd current + vectorY)
        yield x2,y2
    }
    
module Parsing =
    let CoordinateParser =
        pint32 .>> (pchar ',') .>>. pint32
        
    let LineSegmentParser : Parser<LineSegment, unit> =
         CoordinateParser .>> skipString " -> " .>>. CoordinateParser 
    
    let LineSegmentsParser : Parser<LineSegment list, unit> =
        sepEndBy1 LineSegmentParser newline

let solve () =
    let fileContent = File.ReadAllText "5a.txt"
    let lines  = run Parsing.LineSegmentsParser fileContent |> unwrapParserResult
    printfn $"{Seq.length lines} lines"
    
    let filteredLines =
        lines
        |> Seq.filter (fun ((x1,y1),(x2,y2)) ->
                            x1 = x2
                            || y1 = y2
                            || (x1 - x2) = (y1 - y2)
                            || (x1 - x2) = (y2 - y1))
            
    filteredLines
    |> Seq.iter (fun x -> printfn $"{x}")

    let allPoints =
        filteredLines
        |> Seq.collect pointsInLineSegment
        |> Seq.sort
        
    let result =
        allPoints
        |> Seq.countBy id
        |> Seq.filter (fun (_, count) -> count >= 2)
        |> Seq.length
        
    printfn $"{result} result"
    ()