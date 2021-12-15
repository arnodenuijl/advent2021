module Puzzel15a

open System
open System.IO
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
}

type Edge = {
    From: Node
    To: Node
    Costs: Costs
}
//
//    member this.AddNode node costs =
//        this.ConnectedNodes <-
//            this.ConnectedNodes
//            |> Map.add node costs
//            
//    override this.ToString () =
//        let connectedNodesStrings =
//            this.ConnectedNodes
//            |> Map.toSeq
//            |> Seq.map (fun (n,c) -> $"({n.X},{n.Y}): {c}")
//        let connectedNodesString = String.Join(", ", connectedNodesStrings)
//        
//        $"({this.X},{this.Y}) -> {connectedNodesString}"
//    override this.GetHashCode() =
//        HashCode.Combine(this.X, this.Y)

type Trajectory = {
    Path : Node list
    TotalDistance : int32
}
let printTrajectoriesToNodes (trajectoriesToNodes : Map<Node,Trajectory>) =
    trajectoriesToNodes
            |> Map.toSeq
            |> Seq.sortBy (fun (_, trajectory) -> trajectory.TotalDistance)
            |> Seq.iter (fun (node, trajectory) ->
                        let connectedNodesStrings =
                            trajectory.Path
                            |> Seq.map (fun n -> $"({n.X},{n.Y})")
                        let connectedNodesString = String.Join(" -> ", connectedNodesStrings)
                        printfn $"({node.X},{node.Y}) : {trajectory.TotalDistance} : {connectedNodesString}")
    

let solve () =
    let inputText = File.ReadAllText "15a.txt"
    let parser = sepEndBy (many (digit |>> charToInt)) newline
    
    let input = run parser inputText |> unwrapParserResult 
    let xMax = List.length input - 1 
    let yMax = List.length input[0] - 1 
    
    // create nodes and edges
    let nodes, edges = 
        let ns = Array2D.init (xMax + 1) (yMax + 1) (fun x y -> { X = x;Y = y; })
        
        let edges =
            seq {
                for y in [0..yMax] do
                    for x in [0..xMax - 1] do
                        yield {From = ns[x,y]; To=ns[x + 1,y]; Costs =input[x + 1][y]}
                        yield {From = ns[x + 1,y]; To = ns[x,y] ; Costs = input[x][y]} 
                for x in [0..xMax] do
                    for y in [0..yMax - 1] do
                        yield {From = ns[x,y]; To = ns[x,y + 1]; Costs = input[x][y + 1] }
                        yield {From = ns[x,y + 1]; To = ns[x,y]; Costs = input[x][y] } 
            }
            |> List.ofSeq
            |> List.groupBy (fun x -> x.From)
            |> Map.ofList
        ns, edges     
        
    let allNodes =
        seq{
            for x in [0..xMax] do
                for y in [0..yMax] do
                    yield nodes[x,y]
        }
    
    
//    allNodes |> Seq.iter (fun  n -> printfn $"{n}")
    
    let mutable trajectoriesToNodes: Map<Node,Trajectory> =
        allNodes
        |> Seq.map(fun n -> (n, {TotalDistance = Int32.MaxValue; Path = []}))
        |> Map.ofSeq
    
    let updateDistance node distance =
        let currentDistanceToNode = Map.find node trajectoriesToNodes 
        if distance.TotalDistance < currentDistanceToNode.TotalDistance
        then
//            printfn $"Set {node.X},{node.Y} to {distance.TotalDistance}"
            trajectoriesToNodes <- Map.add node distance trajectoriesToNodes

    let mutable visited = Set.empty
    
    let sourceNode = nodes[0,0]
   
    let processNode (node: Node) =
//        printfn $"Process ({node.X},{node.Y})"
//        Console.ReadKey() |> ignore
        visited <- Set.add node visited
        
        let nodesToVisit: Edge list =
            Map.find node edges
            |> List.filter (fun edge -> not <| Set.contains edge.To visited)
        
        let distanceToThisNode = Map.find node trajectoriesToNodes
        
        for edge in nodesToVisit do
            let d = edge.Costs + distanceToThisNode.TotalDistance
            let p = distanceToThisNode.Path @ [edge.To]
            updateDistance edge.To { TotalDistance = d; Path = p }
            
//        printTrajectoriesToNodes trajectoriesToNodes        
        ()
    
    updateDistance sourceNode {Path = [sourceNode] ; TotalDistance=0 }
    let destNode = nodes[xMax,yMax]

    while Set.contains destNode visited |> not do
        let shortestNotVisited =
            trajectoriesToNodes
            |> Map.toSeq
            |> Seq.filter (fun (node, _) -> not <| Set.contains node visited)
            |> Seq.sortBy (fun (_, trajectory) -> trajectory.TotalDistance)
            |> Seq.tryHead
        match shortestNotVisited with
        | Some (node, _) -> processNode node
        | None -> ()

    processNode sourceNode 
    
    
    let trajectoryToDest = Map.find destNode trajectoriesToNodes
    printfn $"{trajectoryToDest.TotalDistance} {trajectoryToDest.Path}"
    ()
    