module Puzzel3b

open Advent2021.Helpers
open System
open System.IO

let solve () = 
    let input =
        File.ReadAllLines "3a.txt"
        |> List.ofArray
                  
    let getDigitsAtPosition pos xs =
        xs
        |> Seq.map (fun (line: string) -> line.ToCharArray().[pos])
        |> Seq.map charToInt

    let filterWithDigitAtPosEquals (digit: int32) (pos: int32) (xs: string list) =
        xs
        |> List.filter (fun line -> digit = (charToInt (line.ToCharArray().[pos])))
        
    let rec step inputs pos digitToKeepWhenEqual compareOperation =
        match inputs with
        | [] -> failwith "lege lijst zou niet moeten kunnen"
        | [a] -> a
        | _ ->  let digits = getDigitsAtPosition pos inputs
                let zeros = digits |> Seq.filter (fun x -> x = 0) |> Seq.length
                let ones = digits  |> Seq.filter (fun x -> x = 1) |> Seq.length
                let digitsToKeep = 
                    if compareOperation zeros ones then 0
                    else 1
                let scrubbedList = filterWithDigitAtPosEquals digitsToKeep pos inputs
                step scrubbedList (pos + 1) digitToKeepWhenEqual compareOperation 
                                
    let oxygen = step input 0 1 (>) |> binaryStringToInt
    let co2 = step input 0 0 (<=) |> binaryStringToInt
    let result = oxygen * co2
    
    printfn $"{oxygen}"
    printfn $"{co2}"
    printfn $"{result}"
    
    ()