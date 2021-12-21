module Puzzel21a

open Microsoft.FSharp.Core

type Turn =
| Player1
| Player2
let solve () =
//    let start = ((4, 0),(8, 0), Player1, 0) // test
    let start = ((1, 0),(3, 0), Player1, 0)
    
    let addToPostion die n =
        let mutable result = die + n
        while result > 10 do
            result <- result - 10
        result
    
    
    let die = Seq.initInfinite (fun i -> i%100 + 1)
    let points =
        die
        |> Seq.chunkBySize 3
        |> Seq.map (Seq.fold (+) 0)

    let ((posP1, scoreP1), (posP2, scoreP2), turn, throws) =
        points
        |> Seq.scan
               (fun ((posP1, scoreP1), (posP2, scoreP2), turn, throws) die ->
                        printfn $"P1 (pos: {posP1}, score: {scoreP1}.  P2 (pos: {posP2}, score: {scoreP2}  "
                        match turn with
                        | Player1 ->
                            let newPos = addToPostion die posP1
                            ((newPos, scoreP1 + newPos), (posP2, scoreP2), Player2, throws + 3)
                        | Player2 ->
                            let newPos = addToPostion die posP2
                            ((posP1, scoreP1), (newPos, scoreP2 + newPos), Player1, throws + 3)
                    )
               start
        |> Seq.find (fun ((posP1, scoreP1), (posP2, scoreP2), _, throws) -> scoreP1 >= 1000 || scoreP2 >= 1000)

    if scoreP1 >= 1000 then
        printfn $"P1 won -> Score P2: {scoreP2}, Throws: {throws}. Result: {scoreP2 * throws}"
    else
        printfn $"P2 won -> Score P1: {scoreP1}, Throws: {throws}. Result: {scoreP1 * throws}"
    ()
    