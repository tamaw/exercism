module Pangram

let alphabet = Set.ofArray (Array.init 26 (fun x -> char (x + int 'A')))
let toCharArray (s:string) = s.ToCharArray()
let toUpper (s:string) = s.ToUpper()

let isPangram (input: string): bool = 
    let letters = input |> toUpper |> toCharArray |> Set.ofArray
    let difference = Set.count (alphabet - letters)
    difference = 0
