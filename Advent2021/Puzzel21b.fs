module Puzzel21b

open System.Threading
open System.Timers
open Microsoft.FSharp.Core

type Turn =
| Player1
| Player2
let solve () =
//    let start = ((4, 0),(8, 0), Player1, 0) // test
    let start = ((1, 0),(3, 0), Player1, 0)
    let addToPostion (d : int) (n : int) =
        let mutable result = d + n
        while result > 10 do
            result <- result - 10
        result
    
    let die = [1;2;3]
    let points =
        seq {
            for d1 in die do
                for d2 in die do
                    for d3 in die do
                        yield d1 + d2 + d3
            }
        |> List.ofSeq
        |> List.groupBy id
        |> List.map (fun (die, items) -> (die, int64 items.Length))
    for (steps, mult) in points do
        printfn $"Steps {steps}, mult {mult}"
    let p1win = ref 0L
    let p2win = ref 0L
    
    use t = new Timer(1000)
    t.Elapsed.Add(fun x ->  printfn $"1: {p1win.Value} 2:{p2win.Value}")
    t.Start()
    let rec playPosition ((posP1, scoreP1), (posP2, scoreP2), turn, throws) (mult: int64): unit =
//        printfn $"P1 (pos: {posP1}, score: {scoreP1}.  P2 (pos: {posP2}, score: {scoreP2}, turns: {turn}, mult: {mult}  "
        for steps, dieMult in points do
            match turn with
            | Player1 ->
                let newPos : int = addToPostion steps posP1
                if scoreP1 + newPos >= 21
                then Interlocked.Add(p1win, (int64 dieMult * int64 mult)) |> ignore
                else
                    playPosition ((newPos, scoreP1 + newPos), (posP2, scoreP2), Player2, throws + 1) (int64 dieMult * mult)
                
            | Player2 ->
                let newPos = addToPostion steps posP2
                if scoreP2 + newPos >= 21
                then Interlocked.Add(p2win, (int64 dieMult * int64 mult)) |> ignore
                else
                    playPosition ((posP1, scoreP1), (newPos, scoreP2 + newPos), Player1, throws + 1) (int64 dieMult * mult)
    playPosition start 1
    
    printfn $"1: {p1win.Value} 2:{p2win.Value}"
    
     