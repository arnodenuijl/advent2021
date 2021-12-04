module Advent2021.Helpers

open System
open FParsec.CharParsers

let charToInt (ch: char)  =
    int ch - int '0'
    
let binaryStringToInt (b: string) =
    Convert.ToInt32(b, 2)

let arrayToString xs =
    xs
    |> Seq.fold
        (fun s x -> $"{s}{x}")
        ""
        
let unwrapParserResult (r : ParserResult<'a, 'b>) =
    match r with
    | Success (result, state, pos) -> result
    | Failure (s ,parserError , userState) ->
        failwith (s) 
