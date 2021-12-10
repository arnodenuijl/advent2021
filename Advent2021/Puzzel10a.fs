module Puzzel10a

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
    
    let findBadCharacter (line:string) : SearchStatus = 
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
        |> snd
    
    let result =
        lines
        |> Seq.map findBadCharacter
        |> Seq.map (function
                    | AllGood -> 0
                    | FoundBadCharacter ')' -> 3
                    | FoundBadCharacter ']' -> 57
                    | FoundBadCharacter '}' -> 1197
                    | FoundBadCharacter '>' -> 25137
                    | FoundBadCharacter x -> failwith $"Kan niks met {x}"
                     )
        |> Seq.sum
        
    
    
    printfn  $"outputValues {result}"