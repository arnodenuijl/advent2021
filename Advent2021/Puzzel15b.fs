module Puzzel15b

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading
open FParsec
open Advent2021.Helpers

let inputText = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"

type Costs = int

type Node = {
    X : int
    Y : int
    Value: int
}

type Edge = {
    From: Node
    To: Node
    Costs: Costs
}

type Trajectory = {
    TotalDistance : int32
}
let printTrajectoriesToNodes (trajectoriesToNodes : Map<Node,Trajectory>) =
    trajectoriesToNodes
            |> Map.toSeq
            |> Seq.sortBy (fun (_, trajectory) -> trajectory.TotalDistance)
            |> Seq.iter (fun (node, trajectory) ->
                        printfn $"({node.X},{node.Y}) : {trajectory.TotalDistance}")
    

let solve () =
    let inputText = File.ReadAllText "15a.txt"
    
    let parser = sepEndBy (many (digit |>> charToInt)) newline
    
    let input = run parser inputText |> unwrapParserResult 
    let xMaxInput = List.length input[0] - 1
    let yMaxInput = List.length input - 1
    
    let xMaxMap = 5 * (List.length input[0]) - 1 
    let yMaxMap = 5 * (List.length input) - 1 
    printfn $"input max: {xMaxInput},{yMaxInput}"
    printfn $"map   max: {xMaxMap},{yMaxMap}"

    let getValueFromOriginalInput x y =
        let extra = x / (xMaxInput + 1) + y / (yMaxInput + 1)
        let inputX = x % (xMaxInput + 1)
        let inputY = y % (yMaxInput + 1)
        let nieuweWaarde =  input[inputY][inputX] + extra
        if nieuweWaarde > 9 then
            (nieuweWaarde % 10) + 1
        else nieuweWaarde

     // create nodes and edges
    let mutable nodes, edgesToProcessPerNode = 
        let ns = Array2D.init (xMaxMap + 1) (yMaxMap + 1) (fun x y -> { X = x;Y = y; Value = getValueFromOriginalInput x y})
        
        let edges =
            seq {
                for y in [0..yMaxMap] do
                    for x in [0..xMaxMap - 1] do
                        yield {From = ns[x,y]; To=ns[x + 1,y]; Costs = ns[(x + 1), y].Value}
                        yield {From = ns[x + 1,y]; To = ns[x,y] ; Costs = ns[x,y].Value} 
                for x in [0..xMaxMap] do
                    for y in [0..yMaxMap - 1] do
                        yield {From = ns[x,y]; To = ns[x,y + 1]; Costs = ns[x,(y + 1)].Value }
                        yield {From = ns[x,y + 1]; To = ns[x,y]; Costs = ns[x,y].Value } 
            }
            |> List.ofSeq
            |> List.groupBy (fun x -> x.From)
            |> Map.ofList
        ns, edges     
       
    let trajectoriesToNodes: Dictionary<Node,Trajectory> =
        Dictionary<Node,Trajectory>()

    let updateDistance node distance =
        match trajectoriesToNodes.TryGetValue(node) with
        | true, t ->
                if distance.TotalDistance < t.TotalDistance
                then
                    trajectoriesToNodes[node] <- distance 
        | false,t->
            trajectoriesToNodes[node] <- distance
        
    let visited = HashSet<Node>()    
    let sourceNode = nodes[0,0]
   
    let q = PriorityQueue<Node, int32>()
    let processNode (node: Node) (value: int32) =
        visited.Add node |> ignore 
        let nodesToVisit: Edge list =
            edgesToProcessPerNode.[node]
            |> List.filter (fun e -> not <| visited.Contains e.To )
        
        for edge in nodesToVisit do
            let d = edge.Costs + value
            updateDistance edge.To { TotalDistance = d }
            q.Enqueue(edge.To, d)
        ()
    
    updateDistance sourceNode {TotalDistance=0 }
    q.Enqueue(sourceNode, 0)
    
    let mutable element : Node = Unchecked.defaultof<Node> 
    let mutable prio : int32 = Unchecked.defaultof<int32> 
    while q.TryDequeue(&element, &prio) do
        if not <| visited.Contains element
        then 
            processNode element prio
        ()
    

    let destNode = nodes[xMaxMap,yMaxMap]
    let trajectoryToDest = trajectoriesToNodes[destNode]
    printfn $"{trajectoryToDest.TotalDistance}"
    ()
    