module Puzzel10b

open System
open System.IO

let inputText = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"


let charPairs =
        [
            ('[', ']')
            ('(', ')')
            ('{', '}')
            ('<', '>')
        ] |> Map.ofSeq

let getClosingChar c =
    Map.find c charPairs

type SearchStatus =
    | AllGood
    | FoundBadCharacter of char
let solve () =
    let inputText = File.ReadAllText "10a.txt"
    let lines = inputText.Split Environment.NewLine
    
    let findBadCharacter (line:string) : (char list * SearchStatus) = 
        line
        |> Seq.fold
            (fun (stack, resultChar) character ->
                match resultChar with
                | FoundBadCharacter _ -> (stack, resultChar) // we already found a bad character
                | AllGood -> 
                    if charPairs |> Map.containsKey character // we have a new opening bracket 
                    then (getClosingChar character :: stack, resultChar) // put it on the stack and continue
                    else // we have a closing bracket
                        match stack with 
                        | [] -> failwith $"Dit moet niet kunnen  ;-)"
                        | head :: rest ->
                            if head = character then
                                (rest, AllGood)
                            else
                                (rest, FoundBadCharacter character)                                                                      
                )
            (List.Empty, AllGood)
    
    let result =
        lines
        |> Array.map findBadCharacter
        |> Array.filter (fun (_ ,searchResult) ->
                        match searchResult with 
                        | AllGood -> true
                        | FoundBadCharacter _ -> false)
        |> Array.map (fun (characters, _) ->
                    characters
                    |> Seq.map (
                                function 
                                | ')' -> 1L
                                | ']' -> 2L
                                | '}' -> 3L
                                | '>' -> 4L
                                | x -> failwith $"no comprendo {x}")    
                    |> Seq.fold
                        (fun state i -> state * 5L + (int64 i))
                        0L
                )
        |> Array.sort
        |> fun items ->
            items |> Seq.iteri (fun i b -> printfn  $"{i} {b}")
            let length = items.Length
            let middle = length / 2
            printfn $"Length = {length}"
            printfn $"Middle = {middle}"
            items[middle]
            
    printfn  $"outputValues {result}"