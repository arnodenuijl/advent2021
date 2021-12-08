module Puzzel8b

open System
open FParsec
open Advent2021.Helpers
open System.IO

let inputText = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

let solve () =
    let inputText = File.ReadAllText "8a.txt"
    let signalsParser =
        sepEndBy (many asciiLetter |>> Set.ofSeq) (pchar ' ')
        |>> List.filter (Set.isEmpty >> not)
    
    let signalsWithOutputParser = signalsParser .>> skipString "| " .>>. signalsParser
    let listOfSignalsWithOutputParser = sepBy signalsWithOutputParser skipNewline
    let listOfsignalsWithOutput = run listOfSignalsWithOutputParser inputText |> unwrapParserResult 
            
    let createMappingFromSetToNumber (sets : Set<char> list) =
        let sets, one = sets |> findSingleWithRest (fun s -> Set.count s = 2) // determined by count of the parts 
        let sets, seven = sets |> findSingleWithRest  (fun s -> Set.count s = 3) // determined by count of the parts
        let sets, four = sets  |> findSingleWithRest  (fun s -> Set.count s = 4) // determined by count of the parts
        let sets, eight = sets  |> findSingleWithRest  (fun s -> Set.count s = 7) // determined by count of the parts
        let sets, nine = sets |> findSingleWithRest (fun s -> Set.isSubset four s) // from the remaining four overlaps only 9
        let sets, three = sets |> findSingleWithRest (fun s -> Set.isSubset s nine && Set.isSubset one s) // from the remaining three fits in nine and 1 fits in three (5 also fits in 9 but 1 doesn't fit in 5)
        let sets, five = sets |> findSingleWithRest (fun s -> Set.isSubset s nine) // from the remaining only 5 fits in nine
        let sets, zero = sets |> findSingleWithRest (fun s -> Set.isSubset one s) // from the remaining one only fits in zero
        let sets, six = sets |> findSingleWithRest (fun s -> Set.isSubset five s) // from the remaining five only fits in six
        let two = Seq.head sets // two is the last one                                              
        [(one, 1);(two, 2);(three, 3);(four, 4);(five, 5);(six, 6);(seven, 7);(eight,8);(nine,9);(zero,0)]
        |> Map.ofSeq        
    
    let numberSeqenceToNumber xs =
        xs
        |> seqToString
        |> Convert.ToInt32
        
    let result =
        listOfsignalsWithOutput
        |> List.map (fun (signals, outputs) ->
            let setToNumber = createMappingFromSetToNumber signals 
            outputs
            |> List.map (fun o -> Map.find o setToNumber)
            |> numberSeqenceToNumber)
        |> List.sum
    printfn  $"outputValues {result}"