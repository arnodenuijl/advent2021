module Puzzel2b

open System.IO
open FParsec

let unwrapParserResult (r : ParserResult<'a, 'b>) =
    match r with
    | Success (result, state, pos) -> result
    | Failure (s ,parserError , userState) ->
        failwith (s) 

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


    let (horizontal,depth, aim) =
        commands
        |> Seq.fold 
            (fun (hor, depth, aim) cmd ->
                match cmd with 
                | Forward x -> (hor + x, depth + aim * x, aim)
                | Down x -> (hor, depth,  aim + x)
                | Up x -> (hor, depth,  aim - x)
            )
            (0, 0, 0)
    let result = horizontal * depth
    printfn $"{result}"
    ()