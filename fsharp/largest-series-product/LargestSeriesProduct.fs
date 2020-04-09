module LargestSeriesProduct
open System

let toCharArray (s:string) = s.ToCharArray()
let charToInt (c:char) = int c - int '0'

let largestProduct (input:string) seriesLength : int option = 
    match (seriesLength, input.Length) with 
    | (sl, l) when sl > l -> None
    | (sl, _) when sl < 0 -> None
    | (0, _) -> Some 1
    | _ when input |> toCharArray |> Array.forall Char.IsDigit |> not -> None
    | _ -> 
        let c = input 
                |> toCharArray 
                |> Array.map charToInt 
                |> Seq.ofArray 
                |> Seq.windowed seriesLength
                |> Seq.map (fun e -> Array.fold (*) 1 e ) 
                |> Seq.max 
        Some c