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


//nice online solution
//let rec findIt (left:List<int>) (right:List<int>) index = 
//    match right with 
//    | [] when left = [] -> index
//    | [x] when left = [] -> index
//    | head::tail when (List.sum left) = (List.sum tail) -> index
//    | head::tail -> findIt (head::left) (tail) (index + 1)
//    | _ -> -1
 
//let findEvenIndex (items:int array) =
    //findIt [] (Array.toList items) 0