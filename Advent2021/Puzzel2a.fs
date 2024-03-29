module Puzzel2a

open System.IO
open FParsec
open Advent2021.Helpers

type SubMovement =
| Forward of int
| Down of int
| Up of int

let ForwardParser : Parser<SubMovement, unit> = 
    skipString "forward " >>. pint32
    |>> Forward

let DownParser : Parser<SubMovement, unit> = 
    skipString "down " >>. pint32
    |>> Down

let UpParser : Parser<SubMovement, unit> = 
    skipString "up " >>. pint32
    |>> Up

let SubParser = choice [
    ForwardParser
    DownParser
    UpParser
]

let solve () = 
    let input = File.ReadAllLines "2a.txt"
    let commands = 
        input
        |> Seq.map (fun line -> run SubParser line |> unwrapParserResult)


    let (horizontal,depth) =
        commands
        |> Seq.fold 
            (fun (hor, depth) cmd ->
                match cmd with 
                | Forward x -> (hor + x,depth)
                | Down x -> (hor,depth + x)
                | Up x -> (hor,depth - x)
            )
            (0,0)
    let result = horizontal * depth
    printfn $"{result}"
    ()