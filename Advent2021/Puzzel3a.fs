module Puzzel3a

open Advent2021.Helpers
open System
open System.IO

let solve () = 
    let input = File.ReadAllLines "3a.txt"
    
    let transpose columnCount (xs: string[]) =
        [0 .. columnCount - 1] 
        |> Seq.map (fun i -> 
            xs
            |> Array.map(fun line -> charToInt line.[i])) // char '0' aftrekken van het character geeft de numerieke waarde
        |> Array.ofSeq

    let mostOccuring (xs : 'a seq) : 'a =
        xs
        |> Seq.countBy id
        |> Seq.sortByDescending snd
        |> Seq.head
        |> fst
    
    let gammaDigits = 
        input
        |> transpose 12
        |> Seq.map mostOccuring
        |> List.ofSeq

    let epsilonDigits = 
        gammaDigits
        |> List.map (fun x -> if x = 0 then 1 else 0)
    
    let gamma =
        gammaDigits
        |> arrayToString
        |> binaryStringToInt
        
    let epsilon =
        epsilonDigits
        |> arrayToString
        |> binaryStringToInt
        
    let result = gamma * epsilon
    printfn $"{result}"
    ()