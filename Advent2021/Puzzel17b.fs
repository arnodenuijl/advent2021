﻿module Puzzel17b

open FSharp.Collections.ParallelSeq

let solve () =
//    let minTargetX, maxTargetX, maxTargetY, minTargetY = 20, 30, -5, -10
    let minTargetX, maxTargetX, maxTargetY, minTargetY = 287, 309, -48, -76

    let tick (x, y, vx, vy) =
        let vx' =
            if vx > 0
            then vx - 1
            else if vx < 0
            then vx + 1
            else 0
        (x + vx, y + vy, vx', vy - 1)
        
    let calculateRoute start =
        let steps =
            Seq.initInfinite id
            |> Seq.scan (fun p _-> tick p) start
            |> Seq.pairwise
            
            // nog niet max x voorbij en nog in beweging
            |> Seq.takeWhile (fun (_, (x2, y2, vx2, vy2)) -> x2 <= maxTargetX && vx2 >= 0) 
            
            // niet in de laatste stap over het blok heen gevlogen (eerste punt links van het blok, tweede punt er rechts van
            |> Seq.takeWhile (fun ((x1, y1, vx1, vy1), (x2, y2, vx2, vy2)) -> not (x1 < minTargetX && x2 > maxTargetX)) 
            
            // niet in de laatste stap over het blok heen gevlogen (eerste punt boven het blok, tweede onder
            |> Seq.takeWhile (fun ((x1, y1, vx1, vy1), (x2, y2, vx2, vy2)) -> not (y1 < minTargetY && y2 > maxTargetY))
            
            // niet een negatieve snelheid en al onder het blok. Dan komen we nooit meer omhoog
            |> Seq.takeWhile (fun ((x1, y1, vx1, vy1), (x2, y2, vx2, vy2)) -> not (vy2 < 0 && y2 < minTargetY))
            |> Seq.map snd
            |> List.ofSeq
        start :: steps
        
    let hitsBox ps =
        ps
        |> List.exists (fun (x, y, _, _) ->
            x >= minTargetX &&
            x <= maxTargetX &&
            y >= minTargetY &&
            y <= maxTargetY )
    
    let ps = calculateRoute (0, 0, 11, -1)
    for p  in ps do
        printfn $"{p} hits {hitsBox ps}"
    
    let paths =
        seq {
            for vx0 in [1..maxTargetX] do
                for vy0 in [minTargetY..200] do
                    yield (vx0, vy0)
        }
        |> PSeq.map (fun (vx0, vy0) -> calculateRoute (0, 0, vx0, vy0))        
        |> PSeq.filter hitsBox
        |> List.ofSeq
    
    for ps in paths do
        let (x, y, vx, vy) = ps[0]
        printfn $"{vx},{vy}"
    printfn $"{Seq.length paths}"
    ()
    