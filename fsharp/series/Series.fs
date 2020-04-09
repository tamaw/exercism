module Series

let slices (str:string) length =
    let slice (l,a,r) (e:char) =
        let a = a @ [e]
        if List.length a >= l then (l, a.[1..], r @ [a |> Array.ofList |> System.String]) else (l, a, r)

    match (length, str.Length) with
    | (0, _) -> None
    | (l, _) when l < 0 -> None
    | (l, sl) when l > sl -> None 
    | (_, sl) when sl = 0 -> None
    | _ -> 
    let (_, _, result) = List.fold slice (length, [], []) (str.ToCharArray() |> List.ofArray) 
    Some result
