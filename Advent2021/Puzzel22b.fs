module Puzzel22b

open System
open System.Numerics
open System.Text
open System.Threading
open Microsoft.FSharp.Core
open FSharp.Collections.ParallelSeq
open Advent2021.Helpers
open FParsec

type Cuboid = {
    OnOrOf : Boolean
    XRange: int * int
    YRange: int * int
    ZRange: int * int
} with
    override c.ToString () =
        $"Cuboid: {c.OnOrOf} x: {fst c.XRange}-{snd c.XRange}, y: {fst c.YRange}-{snd c.YRange}, z: {fst c.ZRange}-{snd c.ZRange}"

type Rectangle = {
    OnOrOf : Boolean
    XRange: int * int
    YRange: int * int
} with
    override p.ToString () =
        $"Rectangle: {p.OnOrOf} x: {fst p.XRange}-{snd p.XRange}, y: {fst p.YRange}-{snd p.YRange}"

type Line = {
    IsOn : Boolean
    Start: int
    End: int
} with
    override l.ToString () =
        $"{l.IsOn} :: {l.Start}-{l.End}"

let getPlanesAtZ z cubes =
    cubes
    |> List.filter (fun c -> fst c.ZRange <= z && snd c.ZRange >= z )
    |> List.map (fun c -> {Rectangle.XRange = c.XRange;Rectangle.YRange=c.YRange;Rectangle.OnOrOf=c.OnOrOf})
    
let getLinesAtX x (planes: Rectangle list)=
    planes
    |> List.filter (fun p -> fst p.XRange <= x && snd p.XRange >= x )
    |> List.map (fun p -> {Line.Start = fst p.YRange;End=snd p.YRange;Line.IsOn=p.OnOrOf})
            
let inline getRange f xs : int * int=
    xs
    |> List.fold
        (fun (min, max) c -> 
            let min = Math.Min(min, fst (f c))
            let max = Math.Max(max, snd (f c))
            (min,max))
        (Int32.MaxValue, Int32.MinValue)
    
let inline getXRange xs = getRange (fun c -> c.XRange) xs 
let inline getYRange xs = getRange (fun c -> c.YRange) xs 
let inline getZRange xs = getRange (fun c -> c.ZRange) xs 
    
// 2d plane
// subtrace line B from line A
let subtractLine (baseLine: Line) (toDelete: Line) : Line list =
    seq {
        if toDelete.Start > baseLine.Start && toDelete.Start < baseLine.End then 
            yield {Line.Start = baseLine.Start; Line.End = toDelete.Start - 1; Line.IsOn = baseLine.IsOn}
        if toDelete.End < baseLine.End && toDelete.End >= baseLine.Start then
            yield {Start = toDelete.End + 1; End = baseLine.End; IsOn = baseLine.IsOn}
        else if toDelete.Start < baseLine.Start && toDelete.End > baseLine.Start then
            if toDelete.End < baseLine.End then
                yield {Start = toDelete.End + 1; End =  baseLine.End; IsOn = baseLine.IsOn}
        else if toDelete.End < baseLine.Start || toDelete.Start > baseLine.End then
            yield {Start = baseLine.Start; End = baseLine.End; IsOn = baseLine.IsOn}           
    }
    |> List.ofSeq        

let addLine (newLine : Line) (knownLines : Line list) =
    let newLines =
        knownLines
        |> List.filter (fun knownline -> knownline.Start <= newLine.End && knownline.End >= newLine.Start )
        |> List.fold
            (fun newLineSegments knownLine ->        
                let newLines =
                    newLineSegments
                    |> List.collect ( fun l -> subtractLine l knownLine)
                newLines
            )
            [newLine]
    knownLines @ newLines

module Parser =
    let xyzRangeParser = pint32 .>> skipString ".." .>>. pint32
    let xParser = skipString " x=" >>. xyzRangeParser
    let yParser = skipString ",y=" >>. xyzRangeParser
    let zParser = skipString ",z=" >>. xyzRangeParser
    let onOrOfParser = choice [
        pstring "on" |>> fun _ -> true
        pstring "off" |>> fun _ -> false
    ]
    let cuboidParser = pipe4 onOrOfParser xParser yParser zParser (fun o x y z -> {OnOrOf=o;XRange=x;YRange=y;ZRange=z})
    
    let cuboidsParser : Parser<Cuboid list, unit>=
        sepBy cuboidParser newline
        
let solve () =
    let inputText = "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"
    printfn $"{subtractLine {Start = 5;End = 10;Line.IsOn = true} {Start = 1;End = 4;Line.IsOn = true}} should be 5-10"
    printfn $"{subtractLine {Start = 5;End = 10;Line.IsOn = true} {Start = 1;End = 5;Line.IsOn = true}} should be 6-10"
    printfn $"{subtractLine {Start = 5;End = 10;Line.IsOn = true} {Start = 4;End = 6;Line.IsOn = true}} should be 7-10"
    printfn $"{subtractLine {Start = 5;End = 10;Line.IsOn = true} {Start = 5;End = 6;Line.IsOn = true}} should be 7-10"
    printfn $"{subtractLine {Start = 5;End = 10;Line.IsOn = true} {Start = 6;End = 8;Line.IsOn = true}} should be 5-5 9-10"
    printfn $"{subtractLine {Start = 5;End = 10;Line.IsOn = true} {Start = 8;End = 10;Line.IsOn = true}} should be 5-7"

//    let cuboids = run Parser.cuboidsParser inputText |> unwrapParserResult
    let cuboids = runParserOnFile Parser.cuboidsParser () "22a.txt" UTF8Encoding.UTF8  |> unwrapParserResult
              
    printfn $"{cuboids.Length}"
    for i in cuboids do
        printfn $"{i}"
    
    let zmin, zmax = getZRange cuboids
    printfn $"Zrange = {zmin} - {zmax}"

    let allRectangleCombinations: List<Rectangle list * int> =
        [zmin..zmax]
        |> Seq.map (fun z -> z, getPlanesAtZ z cuboids)
        |> Seq.countBy snd
        |> List.ofSeq
            
    let totalOn = ref 0L
    let itemsStarted = ref 0
    
    printfn $"{allRectangleCombinations.Length} different rectangle combinations on the Z axis"
    
    allRectangleCombinations
    |> PSeq.iter(fun (rectangles, rectangleCount) ->
        printfn $"{rectangleCount} different Z lines with the same {List.length rectangles} rectangles ({Interlocked.Increment(itemsStarted)}/{allRectangleCombinations.Length})"
        let xMin, xMax = getXRange rectangles
        for x in [xMin..xMax] do
            let linesInAllCubes = getLinesAtX x rectangles
            let resultingLines =
                    List.foldBack
                        (fun (l: Line) allLines -> addLine l allLines)
                        linesInAllCubes
                        []
                    |> List.sortBy ( fun l -> l.Start)
//            let linestring = String.Join(", ", List.map (fun l -> l.ToString()) resultingLines)
//            printfn $"x = {x}. {linestring}"
            let pointsOn =
                resultingLines
                |> List.filter (fun l -> l.IsOn)
                |> List.map (fun l -> 1 + l.End - l.Start)
                |> List.sum
            Interlocked.Add(totalOn, (int64 pointsOn) * (int64 rectangleCount)) |> ignore
        
        printfn  $"{totalOn.Value}")
    printfn  $"{totalOn.Value}"
    
    ()    
     