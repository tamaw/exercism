module AllYourBase

let convertFrom b a = List.mapi (fun i n -> n * (pown b (((List.length a) - 1) - i))) a
let rec convertTo b o n = if n / b = 0 then [n % b] @ o else convertTo b ([n % b] @ o) (n / b)

let rebase digits inputBase outputBase = 
    let validBase b = b >= 2
    let validDigits a b = List.forall(fun n -> n >= 0 && n < b) a

    if validBase inputBase && validBase outputBase && validDigits digits inputBase
    then convertFrom inputBase digits
        |> List.sum
        |> convertTo outputBase [] 
        |> Some
    else None
