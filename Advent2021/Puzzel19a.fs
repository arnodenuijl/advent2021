module Puzzel19a

open System
open System.IO
open System.Numerics
open MathNet.Numerics
open Microsoft.FSharp.Core
open FParsec
open Advent2021.Helpers
open MathNet.Numerics.LinearAlgebra

type Scanner = {
    Number : int32
    Points: Matrix<float>
}
type Displacement = Vector<float>
type RotationName = string
type Rotation = {
    Name: RotationName
    Rotations : Matrix<float> list
}
module Parser =
    let scannerNumber = pstring "--- scanner " >>. pint32 .>> pstring " ---"
    let beaconParser = sepBy pfloat (pchar ',') 
    let beaconsParser: Parser<Matrix<float>,unit> =
        sepBy beaconParser newline
        |>> fun items ->
            items
            |> List.filter (fun xyz -> List.length xyz = 3)
            |> matrix
    let scannerParser =
        scannerNumber .>> newline .>>. beaconsParser
        |>> fun (nr, m) -> { Scanner.Number = nr; Scanner.Points = m }
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
                            [Ry;Rz;Rz]
                            [Ry;Ry;Ry]
                            [Rz;Rz;Rz]
                            [Rx;Rx;Rx;Ry]
                            [Rx;Rx;Ry;Rx]
                            [Rx;Ry;Rx;Rx]
                            [Rx;Ry;Ry;Ry]    
                        ]
                        |> List.mapi (fun i item -> { Name=  $"{i}"; Rotations = item})

let pointDistance (p1: Vector<float>) (p2: Vector<float>): Displacement =
    let dx = p1[0] - p2[0]
    let dy = p1[1] - p2[1]
    let dz = p1[2] - p2[2]
    [dx; dy; dz] |> vector

let rotate (m: Matrix<float>) (rotation : Rotation): Matrix<float> =
    rotation.Rotations
    |> Seq.fold
           (fun m r -> m.Multiply(r))
           m
let solve () =
    let inputText = File.ReadAllText "19a.txt"
    let scanners = run Parser.scannersParser inputText |> unwrapParserResult
    printfn $"{List.length scanners} scanners"
        
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
            
    let findMatchingRotation (s0: Scanner) (s1: Scanner): (Rotation * Displacement) option =
        allRotations
        |> List.map (fun rots ->
            let rotatedMatrix = rotate s1.Points rots
            rots, {s1 with Points = rotatedMatrix} )
        |> List.map (fun (rotation, s1Version) -> rotation, calculatePointsInLine s0.Points s1Version.Points)                    
        |> List.filter (fun (rotation, (displacement, pointsInLine)) ->  pointsInLine >= 12)
        |> List.map (fun (rotation, (displacement, pointsInLine)) ->  (rotation, displacement))
        |> List.tryHead
             
    let scanner0::rest = scanners
    
    let rec FoldWorld (world: Matrix<float>) (rest: Scanner list) =
        let newWorld, newRest =
            rest
            |> List.fold
                    (fun (world,rest) sx ->
                            match findMatchingRotation world sx with
                            | Some rotation, displacement ->
                                let sxPointsInWorldSpace = (rotate sx.Points rotation).Add(displacement)
                                (world :: sxPointsInWorldSpace, rest)
                            | None -> (world, sx::rest))
                    (world , [])
        if newRest <> [] then
            FoldWorld newWorld, newRest
        else
            newWorld
        
    ()
    