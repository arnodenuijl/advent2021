module Puzzel4a

open System.IO
open System.Threading
open FParsec
open Advent2021.Helpers

type BingoCard = int32 option list list

let transpose (xs: 'a list list) =
    let columnCount = xs.[0].Length   
    [0 .. columnCount - 1] 
    |> List.map (fun i -> 
        xs
        |> List.map(fun line -> line.[i])) 
     
let crossOffNumber (bingoCard: BingoCard) numberToCrossOff : BingoCard =
    bingoCard
    |> List.map (fun row ->
            row
            |> List.map (fun currentValue -> 
                        match currentValue with 
                        | Some number when number = numberToCrossOff -> None
                        | _ -> currentValue))

let hasBingo bingoCard =
    let bingoInRows =
        bingoCard
        |> List.exists (List.exists Option.isSome >> not)
        
    let bingoInColumns =
        bingoCard
        |> transpose
        |> List.exists (List.exists Option.isSome >> not)

    bingoInRows || bingoInColumns

let getAllNumbersLeft bingoCard =
    seq {
        for row in bingoCard do
            for item in row do
                match item with
                | Some x -> yield x
                | None -> ()
    }
    
module Parsing = 
    let DrawnNumbersParser : Parser<int32 list, unit> =
        sepBy pint32 (pchar ',') .>> newline

    let NumberOptionParser : Parser<int32 option, unit> =
        skipMany (pchar ' ') >>. pint32 |>> Some

    let BingoCardRowParser : Parser<int32 option list, unit> =
        sepBy1 NumberOptionParser (many1 (pchar ' '))

    let BingoCardParser : Parser<BingoCard, unit> =
        sepEndBy1 BingoCardRowParser newline
        
    let BingoCardsParser : Parser<BingoCard list, unit> =
        sepEndBy1 BingoCardParser newline
        
    let InputParser =
        DrawnNumbersParser .>> newline .>>. BingoCardsParser

let getBingoStream () = 
    let fileContent = File.ReadAllText "4a.txt"
    let drawnNumbers, bingoCards = run Parsing.InputParser fileContent |> unwrapParserResult
    printfn $"{bingoCards.Length} cards"
    printfn $"{drawnNumbers.Length} drawn numbers"
    
    drawnNumbers
    |> Seq.scan (fun (cardsWithBingo, restBingoCards, previousDrawnNumber) drawnNumber ->
            let updatedBingoCards =
                restBingoCards
                |> List.map (fun bingoCard -> crossOffNumber bingoCard drawnNumber)
                
            let cardsWithBingo, restBingoCards =
                updatedBingoCards
                |> List.fold
                    (fun (cardsWithBingo, restBingoCards) bingoCard ->
                        if hasBingo bingoCard then
                            bingoCard :: cardsWithBingo, restBingoCards
                        else
                            cardsWithBingo, bingoCard :: restBingoCards)
                    ([],[])
            (cardsWithBingo, restBingoCards, drawnNumber)
        )      
        ([], bingoCards, 0)
    |> Seq.collect (fun (cardsWithBingo, _, drawnNumber) ->
                        cardsWithBingo
                        |> List.map (fun cardWithBingo -> cardWithBingo, drawnNumber))

let solve () =
    let bingoStream = getBingoStream ()
    
    let firstBingoCard, lastDrawnNumber =
        bingoStream
        |> Seq.head
    let numbersLeft = getAllNumbersLeft firstBingoCard
    let result4a = lastDrawnNumber * (numbersLeft |> Seq.sum)   
    printfn $"Result 4a: {result4a}"

    ()