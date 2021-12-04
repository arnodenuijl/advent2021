module Puzzel4b

let solve () = 
    let bingoStream = Puzzel4a.getBingoStream ()
    let firstBingoCard, lastDrawnNumber =
        bingoStream
        |> Seq.filter (fun (bingoCard, _) -> Puzzel4a.hasBingo bingoCard)
        |> Seq.last
    let numbersLeft = Puzzel4a.getAllNumbersLeft firstBingoCard
    let result4b = lastDrawnNumber * (numbersLeft |> Seq.sum)   
    printfn $"Result 4b: {result4b}"
    ()