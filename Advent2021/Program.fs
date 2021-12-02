// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions

[<EntryPoint>]
let main args =
    match args with 
    | [|"1a"|] -> Puzzel1a.solve ()
    | [|"1b"|] -> Puzzel1b.solve ()
    | [|"2a"|] -> Puzzel2a.solve ()
    | [|"2b"|] -> Puzzel2b.solve ()
    | [|"3a"|] -> Puzzel3a.solve ()
    | [|"3b"|] -> Puzzel3b.solve ()
    | [|"4a"|] -> Puzzel4a.solve ()
    | _ -> Console.WriteLine "niets te doen"
    0
