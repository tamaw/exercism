module SumOfMultiples

let rec calc multiples upper n i =
    let v = n * i
    if (v < upper) then
        calc (Set.add v multiples) upper n (i + 1)
    else
        multiples

// nest recursive functions, assignments can have if then else

let rec dd multiples (numbers: int list) i =
    if (i < numbers.Length) then
        dd multiples numbers (i+1)
    else
        multiples

let sum (numbers: int list) (upperBound: int): int = 
    //let f = List.filter (fun x -> x < upperBound) numbers
    //let ff = seq { for i in 1 .. upperBound -> numbers.[0] * i}
    let multiples = dd Set.empty numbers 


    //let multiples = calc Set.empty upperBound numbers.[0] 0
    //let multiples = calc multiples upperBound numbers.[1] 0
    //let fa = List.init

    //let fff = [ for i in 0 .. v1 -> i * v1 ]
    // <= v1 && i * v1 < upperBound -> i * v1 ]
    List.sum (Set.toList multiples)

