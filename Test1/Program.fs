// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    [|30.0; 60.0; 90.0|] 
    |> Array.map (fun a -> (a, a+a))
    |> fun result -> match result.[0] with
                     | (_, b) -> b
    |> fun twoa -> printfn "%f" twoa
    0
