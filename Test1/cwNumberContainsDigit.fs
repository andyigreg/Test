module cwNumberContainsDigit

let numbersWithDigitInside (x : int64) (d : int64) = 
    let matches =[int64 1..x] |> Seq.filter(fun n -> n = d)
    [Seq.length matches |> int64; Seq.sum matches |> int64; (Seq.reduce (fun x y -> x * y) matches) |> int64]