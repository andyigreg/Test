module cwMaxDifference

let mxdiflg(a1: string[]) (a2: string[]): int Option = 
    match a1, a2 with
        | [||], _ -> None
        | _, [||] -> None
        | _, _ -> 
                  let crossproduct l1 l2 =
                      seq { for el1 in l1 do
                              for el2 in l2 do
                                yield abs (el1 - el2) }

                  let a1len = Array.map (fun a -> String.length a) a1
                  let a2len = Array.map (fun a -> String.length a) a2

                  crossproduct a1len a2len |> Seq.max |> Some


//let mxdiflg(a1: string[]) (a2: string[]): int Option = 
    //match a1, a2 with
    //| _ when Array.isEmpty a1 || Array.isEmpty a2 -> None
    //| left, right -> 
        //Seq.allPairs left right 
        //|> Seq.map (fun (str1, str2) -> abs((String.length str1) - (String.length str2))) 
        //|> Seq.max |> Some

