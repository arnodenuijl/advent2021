module Puzzel19b

open System
open System.Collections.Immutable
open System.IO
open MathNet.Numerics.LinearAlgebra.Double
open Microsoft.FSharp.Core
open FParsec
open Advent2021.Helpers
open MathNet.Numerics.LinearAlgebra
open Puzzel19a

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
            
    let movePoints (vector: Vector<float>) (s: Scanner) : Scanner=
        { s with Points = s.Points.MapIndexed(fun x y f -> f + vector[y]) } 
    
    let findMatchingRotation (s0: Scanner) (s1: Scanner): (Scanner * Rotation * Displacement) option =
        let possibleRotations =
            allRotations
            |> List.collect (fun rotation ->
                            let rotatedMatrix = rotate rotation s1
                            let displacement, pointsInline = calculatePointsInLine s0.Points rotatedMatrix.Points
                            if pointsInline >= 12 then
                                [
                                    s1, rotation, displacement
                                ]
                            else []
                        )
        if List.length possibleRotations > 1 then
            failwith $"Expected 1 possible rotation. Maar is {List.length possibleRotations}"
        else if List.length possibleRotations = 1 then
            Some possibleRotations[0]
        else
            None
            
    let deduplicate (m:Matrix<float>) : Matrix<float>=
        m.EnumerateRows()
        |> Seq.fold
               (fun (dd: ImmutableHashSet<Vector<float>>) (i: Vector<float>) -> dd.Add i)
               (ImmutableHashSet<Vector<float>>.Empty)
        |> matrix

    let rec findMatches (sourceScanner:Scanner) (scannersToMatch:Scanner list) =
        let mutable matched = [(sourceScanner, [], [0.0;0.0;0.0] |> vector)]
        let mutable toMatch = scannersToMatch
        
        while toMatch <> [] do
            printfn $"To match {List.length toMatch} matched {List.length matched}"
            for scannerToMatch in toMatch do
                printfn $"Start match {scannerToMatch.Name}"
                let mutable found = false
                for sourceScanner in matched do
                    if not found then
                        let scan, rot, disp = sourceScanner
                        let adjustedScanner =
                            scan
                            |> rotate rot
                            
                        match findMatchingRotation adjustedScanner scannerToMatch with
                        | Some (s: Scanner, r: Rotation, d: Displacement) ->
                            printfn $"Matched {s.Name}"                             
                            matched <- (s,r,d.Add(disp)) :: matched
                            toMatch <- List.filter (fun s -> s <> scannerToMatch) toMatch
                            found <- true
                        | None -> ()
        matched
                        
            
    let inputText = File.ReadAllText "19a.txt"
    let scanners = run Parser.scannersParser inputText |> unwrapParserResult
        
    let scanner0 = List.find (fun s -> s.Name = "0") scanners
    let rest = List.filter (fun s -> s.Name <> "0") scanners
        
    let scannersWithDisplacement =
        findMatches scanner0 rest
        |> List.ofSeq
    
    let manhattenDistance (l1: Vector<float>) (l2: Vector<float>) =
        Math.Abs(l1[0] - l2[0]) +
        Math.Abs(l1[1] - l2[1]) +
        Math.Abs(l1[2] - l2[2])
        
    let result =
        seq {
            for s1 in scannersWithDisplacement do
                for s2 in scannersWithDisplacement do
                    let s1,rot1,dist1 = s1
                    let s2,rot2,dist2 = s2
                    let d = manhattenDistance dist1 dist2
                    printfn  $"{s1.Name} ({dist1[0]},{dist1[1]},{dist1[2]}) to {s2.Name} ({dist2[0]},{dist2[1]},{dist2[2]}) = {d}"
                    d
        }
        
        |> Seq.max
    printfn $"{result} = 10569"    
    ()
    