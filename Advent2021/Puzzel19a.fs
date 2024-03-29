﻿module Puzzel19a

open System.Collections.Immutable
open System.IO
open MathNet.Numerics.LinearAlgebra.Double
open Microsoft.FSharp.Core
open FParsec
open Advent2021.Helpers
open MathNet.Numerics.LinearAlgebra

type Scanner = {
    Name: string
    Points: Matrix<float>
}

type Displacement = Vector<float>
type Rotation = Matrix<float> list

module Parser =
    let scannerNumber = pstring "--- scanner " >>. pint32 .>> pstring " ---"
    let beaconParser = sepBy pfloat (pchar ',') 
    let beaconsParser: Parser<Matrix<float>,unit> =
        sepBy beaconParser newline
        |>> fun items ->
            items
            |> List.filter (fun xyz -> List.length xyz = 3)
            |> matrix
    let scannerParser: Parser<Scanner ,unit>=
        scannerNumber .>> newline .>>. beaconsParser
        |>> fun (name, points) -> { Name = name.ToString(); Points = points }
        
    let scannersParser = sepEndBy scannerParser (many newline)
    
 
let Rx = matrix [
                    [1.0;0.0;0.0]
                    [0.0;0.0;-1.0]
                    [0.0;1.0;0.0]
                ]
let Ry = matrix [
                    [0.0;0.0;1.0]
                    [0.0;1.0;0.0]
                    [-1.0;0.0;0.0]
                ]
let Rz = matrix [
                    [0.0;-1.0;0.0]
                    [1.0;0.0;0.0]
                    [0.0;0.0;1.0]
        ]


let allRotations: Rotation list =
                        [
                            []
                            [Rx]
                            [Ry]
                            [Rz]
                            [Rx;Rx]
                            [Rx;Ry]
                            [Rx;Rz]
                            [Ry;Rx]
                            [Ry;Ry]
                            [Rz;Ry]
                            [Rz;Rz]
                            [Rx;Rx;Rx]
                            [Rx;Rx;Ry]
                            [Rx;Rx;Rz]
                            [Rx;Ry;Rx]
                            [Rx;Ry;Ry]
                            [Rx;Rz;Rz]
                            [Ry;Rx;Rx]
                            [Ry;Ry;Ry]
                            [Rz;Rz;Rz]
                            [Rx;Rx;Rx;Ry]
                            [Rx;Rx;Ry;Rx]
                            [Rx;Ry;Rx;Rx]
                            [Rx;Ry;Ry;Ry]    
                        ]
                        |> List.mapi (fun i item -> item)

let pointDistance (p1: Vector<float>) (p2: Vector<float>): Displacement =
    let dx = p1[0] - p2[0]
    let dy = p1[1] - p2[1]
    let dz = p1[2] - p2[2]
    [dx; dy; dz] |> vector

let rotate (rotation : Rotation) (m: Scanner) : Scanner =
    let newPoints =
        rotation
        |> Seq.fold
            (fun (m: Matrix<float>) r -> m.Multiply(r))
            m.Points
    { m with Points = newPoints }
let solve () =
        
    let calculatePointsInLine (m1: Matrix<float>) (m2: Matrix<float>): Displacement * int =
        seq {
            for p1 in [0..m1.RowCount - 1] do
                for p2 in [0..m2.RowCount - 1] do
                    let r1 = m1.Row p1
                    let r2 = m2.Row p2
                    let d = pointDistance r1 r2
                    yield (d, p1, p2)
        }
        |> Seq.toList
        |> List.countBy (fun (distance, _, _) -> distance)
        |> List.sortByDescending (fun (_, count) -> count)
        |> List.head
            
    let movePoints (vector: Vector<float>) (m: Matrix<float>) : Matrix<float>=
        m.MapIndexed(fun x y f -> f + vector[y])
    
    let findMatchingRotation (s0: Scanner) (s1: Scanner): Scanner option =
        allRotations
        |> Seq.collect (fun rotation ->
                            let rotatedMatrix = rotate rotation s1
                            let displacement, pointsInline = calculatePointsInLine s0.Points rotatedMatrix.Points
                            printfn $"S1: {s0.Name} S2: {s1.Name} pointsInline: {pointsInline}"
                            if pointsInline >= 12 then
                                [
                                    { rotatedMatrix with Points = movePoints displacement rotatedMatrix.Points }
                                ]
                            else []
                        )
        |> Seq.tryHead
               
    let deduplicate (m:Matrix<float>) : Matrix<float>=
        m.EnumerateRows()
        |> Seq.fold
               (fun (dd: ImmutableHashSet<Vector<float>>) (i: Vector<float>) -> dd.Add i)
               (ImmutableHashSet<Vector<float>>.Empty)
        |> matrix

    let rec findMatches (sourceScanner:Scanner) (scannersToMatch:Scanner list) =
        let mutable matched = [sourceScanner]
        let mutable toMatch = scannersToMatch
        
        while toMatch <> [] do
            printfn $"To match {List.length toMatch} matched {List.length matched}"
            for scannerToMatch in toMatch do
                printfn $"Start match {scannerToMatch.Name}"
                let mutable found = false
                for sourceScanner in matched do
                    if not found then
                        match findMatchingRotation sourceScanner scannerToMatch with
                        | Some s ->
                            printfn $"Matched {s.Name}"
                            matched <- s :: matched
                            toMatch <- List.filter (fun s -> s <> scannerToMatch) toMatch
                            found <- true
                        | None -> ()
        matched
                        
            
    let inputText = File.ReadAllText "19a.txt"
    let scanners = run Parser.scannersParser inputText |> unwrapParserResult
    
    printfn $"{List.length scanners} scanners"
    for s in scanners do
        printfn $"{s.Name} : {s.Points.RowCount} rows" 
    
    let scanner0 = List.find (fun s -> s.Name = "10") scanners
    let rest = List.filter (fun s -> s.Name <> "10") scanners
    let matches = findMatches scanner0 rest
    
    let merged =
        matches
        |> Seq.collect (fun m -> m.Points.EnumerateRows())
        |> matrix
        |> deduplicate
    
    printfn  $"{merged.RowCount}"
    ()
    