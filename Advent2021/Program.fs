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
    | [|"8a"|] -> Puzzel8a.solve ()
    | [|"8b"|] -> Puzzel8b.solve ()
    | [|"9a"|] -> Puzzel9a.solve ()
    | [|"9b"|] -> Puzzel9b.solve ()
    | [|"10a"|] -> Puzzel10a.solve ()
    | [|"10b"|] -> Puzzel10b.solve ()
    | [|"11a"|] -> Puzzel11a.solve ()
    | [|"11b"|] -> Puzzel11b.solve ()
    | [|"12a"|] -> Puzzel12a.solve ()
    | [|"12b"|] -> Puzzel12b.solve ()
    | [|"13a"|] -> Puzzel13a.solve ()
    | [|"13b"|] -> Puzzel13b.solve ()
    | [|"14a"|] -> Puzzel14a.solve ()
    | [|"14b"|] -> Puzzel14b.solve ()
    | _ -> Console.WriteLine "niets te doen"
    0
