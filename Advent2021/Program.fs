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
    | [|"4b"|] -> Puzzel4a.solve ()
    | [|"5a"|] -> Puzzel5a.solve ()
    | [|"5b"|] -> Puzzel5b.solve ()
    | [|"6a"|] -> Puzzel6a.solve ()
    | [|"6b"|] -> Puzzel6b.solve ()
    | [|"7a"|] -> Puzzel7a.solve ()
    | [|"7b"|] -> Puzzel7b.solve ()
    | _ -> Console.WriteLine "niets te doen"
    0
