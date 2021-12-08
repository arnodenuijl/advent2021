module Advent2021.Helpers

open System
open FParsec.CharParsers

let charToInt (ch: char)  =
    int ch - int '0'
    
let binaryStringToInt (b: string) =
    Convert.ToInt32(b, 2)

let seqToString (xs: 'a seq) =
    String.Join("", xs)

// Find an item in a list and return the rest of the list and the item
// Must find 1 and only 1 item or an exception is thrown
let findSingleWithRest (f : 'a -> bool) (xs : 'a list) : 'a list * 'a =       
    List.foldBack
        (fun item (restList, foundItem)  ->
            match foundItem, f item with
            | Some _ , true -> failwith "found multiple items"
            | Some _ , false -> (item :: restList, foundItem)
            | None , true -> (restList, Some item)
            | None , false -> (item :: restList, foundItem)
            )
        xs
        ([], None)      
    |> function
        | rest, Some found -> (rest, found)
        | _, None -> failwith "not found"
        
let unwrapParserResult (r : ParserResult<'a, 'b>) =
    match r with
    | Success (result, state, pos) -> result
    | Failure (s ,parserError , userState) ->
        failwith (s) 
