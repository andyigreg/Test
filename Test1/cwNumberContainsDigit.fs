module cwNumberContainsDigit

let numbersWithDigitInside (x : int64) (d : int64) = 
    let matches =[int64 1..x] |> Seq.filter(fun n -> string n |> Seq.contains (string d).[0])
    if Seq.isEmpty matches then [0L; 0L; 0L] else [Seq.length matches |> int64; Seq.sum matches |> int64; (Seq.reduce (fun x y -> x * y) matches) |> int64]