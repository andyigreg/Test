module persistence

let rec persistence n = 
  let rec persrecursive count num =
    match num with
      | n when n <= 9 -> count
      | _ -> sprintf "%i" num |> 
             Seq.toList |> 
             List.map (fun numstr -> int numstr - int '0' ) |> 
             List.reduce (fun x y -> x * y) |> 
             persrecursive (count + 1)
  persrecursive 0 n