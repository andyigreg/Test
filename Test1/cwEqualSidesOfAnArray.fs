module cwEqualSidesOfAnArray

let findEvenIndex (items : int array) = 
    let itemsList = items |> List.ofArray
    let rec findIndex left right index =
      match right with
        | x::xs -> if List.sum xs = List.sum left then index else findIndex (x::left) xs (index+1)
        | [] -> -1

    match itemsList with
      | [] -> 0
      | _ -> findIndex [] itemsList 0