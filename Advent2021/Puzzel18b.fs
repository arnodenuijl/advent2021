module Puzzel18b

open System
open System.IO
open Microsoft.FSharp.Core
open FParsec
open Advent2021.Helpers
open FSharp.Collections.ParallelSeq

type Node =
    | SubNode of Left: Node * Right: Node
    | Value of int
with
    override this.ToString() =        
        let rec print (n: Node) =
            match n with
            | SubNode(left, right) -> $"[{print left},{print right}]"
            | Value i -> $"{i}"
        print this
let add (a:Node) (b:Node) =
    SubNode(a,b)
        
type ExplosionValues = {
    Left : int
    Right : int
}

type TryExplodeResult =
| Exploded of Node * ExplosionValues
| NotExploded of Node
with
    override this.ToString() =        
        let rec print (n: TryExplodeResult) =
            match n with
            | Exploded(node, exp) -> $"[{node},{exp}]"
            | NotExploded node -> $"Not exploded: {node}"
        print this


type TrySplitResult =
| Split of Node
| NotSplit of Node


module Parser =
    let subNodeOrValueParser, subNodeOrValueParserRef = createParserForwardedToRef<Node, unit>()
    let valueParser: Parser<Node, unit> = pint32 |>> Value
    let pairParser = subNodeOrValueParser .>> pchar ',' .>>. subNodeOrValueParser |>> SubNode

    let subNodeParser : Parser<Node, unit> = between (pstring "[") (pstring "]") pairParser
    
    subNodeOrValueParserRef.Value <- choice [valueParser;subNodeParser]
    let nodesParser = sepBy subNodeOrValueParser newline
    
            
let solve () =
    
    let div2RoundUp (i: int) =
        Math.Ceiling (float i / 2.0) |> int
        
    let div2RoundDown (i: int) =
        Math.Floor (float i / 2.0) |> int              
        
    let rec addValueMostLeft (entry: Node, v:int): Node =
        match entry with
        | Value i ->
            let newEntry = Value (i + v)
            newEntry
        | SubNode (l,r) ->
            let newL = addValueMostLeft(l, v)
            SubNode(newL, r)
    
    and addValueMostRight (entry: Node, v:int): Node=
        match entry with
        | Value i ->
            let newEntry = Value (i + v)
            newEntry
        | SubNode (l,r) ->
            let newR = addValueMostRight(r, v)
            SubNode(l, newR)
    
    let rec trySplit (entry: Node) : TrySplitResult =
        match entry with
        | Value i when i >= 10 ->
//            printfn $"Split {i} into [{div2RoundDown i},{div2RoundUp i}]"
            Split(SubNode(Value (div2RoundDown i), Value (div2RoundUp i)))
        | Value _ -> NotSplit entry
        | SubNode(left, right) ->
            match trySplit left with
            | Split n -> Split (SubNode(n, right))
            | NotSplit _ ->
                match trySplit right with
                | Split n -> Split (SubNode(left, n))
                | NotSplit _ -> NotSplit entry
            
    let rec tryExplode (depth: int) (entry: Node): TryExplodeResult =
        match entry with
        | Value i  -> NotExploded (Value i)
        | SubNode (Value l, Value r) when depth >= 4 ->
//            printfn $"Explode {l},{r}"
            Exploded (Value 0, {Left = l;Right = r })
        | SubNode (l, r) ->
            match tryExplode (depth + 1) l with
            | Exploded (newL, ev) -> // links explode
                let newR = addValueMostLeft(r, ev.Right)
                Exploded (SubNode(newL, newR), { Left = ev.Left; Right = 0 })
            | NotExploded newL ->
                match tryExplode (depth + 1) r with
                | NotExploded newR -> NotExploded (SubNode(newL, newR))
                | Exploded (newR, ev) -> // rechts explode
                    let newL = addValueMostRight(newL, ev.Left) // links de waarde toevoegen
                    Exploded (SubNode(newL, newR), { Left = 0; Right = ev.Right })
    
    let rec calculateMagnitude n =
        match n with
        | Value i -> i
        | SubNode(left, right) -> 3 * (calculateMagnitude left) + 2 * (calculateMagnitude right)  
    
    let rec processUntilDone (n: Node): Node =
        printfn $"{n}"
        match tryExplode 0 n with
        | Exploded (n: Node, _) -> processUntilDone n
        | NotExploded n ->
            match trySplit n with
            | Split n -> processUntilDone n
            | NotSplit n-> n

    let inputText = File.ReadAllText "18a.txt"
    let numbersToAdd = run Parser.nodesParser inputText |> unwrapParserResult
    let maxindex = List.length numbersToAdd - 1
    let allcombos = seq {
        for a in [0..maxindex] do
            for b in [0..maxindex] do
                yield (numbersToAdd[a], numbersToAdd[b]) 
    }        
    let result =
        allcombos
        |> PSeq.map (fun (a,b) -> add a b)
        |> PSeq.map processUntilDone
        |> PSeq.map calculateMagnitude
        |> PSeq.max
    printfn $"{result}"
    ()
    